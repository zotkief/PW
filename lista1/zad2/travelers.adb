with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Seeds; use Random_Seeds;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Characters.Handling; use Ada.Characters.Handling;


procedure  Travelers is


-- Travelers moving on the board

  Nr_Of_Travelers : constant Integer :=30;

  Min_Steps : constant Integer := 10 ;
  Max_Steps : constant Integer := 100 ;

  Min_Delay : constant Duration := 0.01;
  Max_Delay : constant Duration := 0.05;

-- 2D Board with torus topology

  Board_Width  : constant Integer := 15;
  Board_Height : constant Integer := 15;

-- Timing

  Start_Time : Time := Clock;  -- global startnig time

-- Random seeds for the tasks' random number generators
 
  Seeds : Seed_Array_Type(1..Nr_Of_Travelers) := Make_Seeds(Nr_Of_Travelers);

-- Types, procedures and functions

  -- Postitions on the board
  type Position_Type is record	
    X: Integer range 0 .. Board_Width - 1; 
    Y: Integer range 0 .. Board_Height - 1; 
  end record;

  protected type Mutex is
   entry Lock;
   procedure Unlock;
private
   Locked : Boolean := False;
end Mutex;

protected body Mutex is
   entry Lock when not Locked is
   begin
      Locked := True;
   end Lock;

   procedure Unlock is
   begin
      Locked := False;
   end Unlock;
end Mutex;

type Mutex_Array is array (Integer range <>, Integer range <>) of Mutex;

Board : Mutex_Array(0 .. Board_Width-1, 0 .. Board_Height-1);	   

  -- elementary steps
  function Move_Down( Position : in Position_Type ) return Position_Type is
   New_Position : Position_Type := Position;
   begin
      New_Position.Y := ( Position.Y + 1 ) mod Board_Height;
      return New_Position;
   end Move_Down;


  function Move_Up( Position : in Position_Type ) return Position_Type is
   New_Position : Position_Type := Position;
   begin
      New_Position.Y := ( Position.Y - 1 + Board_Height ) mod Board_Height;
      return New_Position;
   end Move_Up;


  function Move_Right( Position : in Position_Type ) return Position_Type is
   New_Position : Position_Type := Position;
   begin
      New_Position.X := ( Position.X + 1 ) mod Board_Width;
      return New_Position;
   end Move_Right;


  function Move_Left( Position : in Position_Type ) return Position_Type is
   New_Position : Position_Type := Position;
   begin
      New_Position.X := ( Position.X - 1 +Board_Width ) mod Board_Width;
      return New_Position;
   end Move_Left;


  -- traces of travelers
  type Trace_Type is record 	      
    Time_Stamp:  Duration;	      
    Id : Integer;
    Position: Position_Type;      
    Symbol: Character;	      
  end record;	      

  type Trace_Array_type is  array(0 .. Max_Steps) of Trace_Type;

  type Traces_Sequence_Type is record
    Last: Integer := -1;
    Trace_Array: Trace_Array_type ;
  end record; 


  procedure Print_Trace( Trace : Trace_Type ) is
    Symbol : String := ( ' ', Trace.Symbol );
  begin
    Put_Line(
        Duration'Image( Trace.Time_Stamp ) & " " &
        Integer'Image( Trace.Id ) & " " &
        Integer'Image( Trace.Position.X ) & " " &
        Integer'Image( Trace.Position.Y ) & " " &
        ( ' ', Trace.Symbol ) -- print as string to avoid: '
      );
  end Print_Trace;

  procedure Print_Traces( Traces : Traces_Sequence_Type ) is
  begin
    for I in 0 .. Traces.Last loop
      Print_Trace( Traces.Trace_Array( I ) );
    end loop;
  end Print_Traces;

  -- task Printer collects and prints reports of traces
  task Printer is
    entry Report( Traces : Traces_Sequence_Type );
  end Printer;
  
  task body Printer is 
  begin
    for I in 1 .. Nr_Of_Travelers loop -- range for TESTS !!!
        accept Report( Traces : Traces_Sequence_Type ) do
          Print_Traces( Traces );
        end Report;
      end loop;
  end Printer;


  -- travelers
  type Traveler_Type is record
    Id: Integer;
    Symbol: Character;
    Position: Position_Type;    
  end record;


  task type Traveler_Task_Type is	
    entry Init(Id: Integer; Seed: Integer; Symbol: Character);
    entry Start;
  end Traveler_Task_Type;	

  task body Traveler_Task_Type is
    G : Generator;
    Traveler : Traveler_Type;
    Time_Stamp : Duration;
    Nr_of_Steps: Integer;
    Traces: Traces_Sequence_Type; 
    accepter: Boolean;

    procedure Store_Trace is
    begin  
      Traces.Last := Traces.Last + 1;
      Traces.Trace_Array( Traces.Last ) := ( 
          Time_Stamp => Time_Stamp,
          Id => Traveler.Id,
          Position => Traveler.Position,
          Symbol => Traveler.Symbol
        );
    end Store_Trace;
    
    function Make_Step return Boolean is
      N : Integer; 
      tempPos : Position_Type;
      continuer : Boolean:=true;
    begin 
      N := Integer( Float'Floor(4.0 * Random(G)) );    
      case N is
        when 0 =>
          tempPos:=Move_Up( Traveler.Position );
        when 1 =>
          tempPos:=Move_Down( Traveler.Position );
        when 2 =>
          tempPos:=Move_Left( Traveler.Position );
        when 3 =>
          tempPos:=Move_Right( Traveler.Position );
        when others =>
          Put_Line( " ?????????????? " & Integer'Image( N ) );
        end case;

      select
         Board(tempPos.X,tempPos.Y).Lock;
      or
         delay Duration(2*Max_Delay);
         continuer:=false;
      end select;


      if continuer=false then
         return false;
      end if;



      Board(Traveler.Position.X,Traveler.Position.Y).Unlock;

      Traveler.Position.X:=tempPos.X;
      Traveler.Position.Y:=tempPos.Y;
      
      Time_Stamp := To_Duration ( Clock - Start_Time ); -- reads global clock
      Store_Trace;


      return true;
    end Make_Step;
  begin
    accept Init(Id: Integer; Seed: Integer; Symbol: Character) do
      Reset(G, Seed); 
      Traveler.Id := Id;
      Traveler.Symbol := Symbol;
      -- Random initial position:
      Traveler.Position := (
          X => Integer( Float'Floor( Float( Board_Width )  * Random(G)  ) ),
          Y => Integer( Float'Floor( Float( Board_Height ) * Random(G) ) )          
        );
      Store_Trace; -- store starting position
      -- Number of steps to be made by the traveler  
      Nr_of_Steps := Min_Steps + Integer( Float(Max_Steps - Min_Steps) * Random(G));
      -- Time_Stamp of initialization
      Time_Stamp := To_Duration ( Clock - Start_Time ); -- reads global clock
    end Init;
    
    -- wait for initialisations of the remaining tasks:
    accept Start do
      null;
    end Start;

    Board(Traveler.Position.X,Traveler.Position.Y).Lock;

    for Step in 0 .. Nr_of_Steps loop
      delay Min_Delay+(Max_Delay-Min_Delay)*Duration(Random(G));
      -- do action ...
      accepter:=Make_Step;

      if accepter=false then
         Traveler.Symbol := Ada.Characters.Handling.To_Lower(Traveler.Symbol);

         Store_Trace;
         exit;
      end if;
    end loop;
    Printer.Report( Traces );
  end Traveler_Task_Type;


-- local for main task

  Travel_Tasks: array (0 .. Nr_Of_Travelers-1) of Traveler_Task_Type; -- for tests
  Symbol : Character := 'A';
begin 
  
  -- Prit the line with the parameters needed for display script:
  Put_Line(
      "-1 "&
      Integer'Image( Nr_Of_Travelers ) &" "&
      Integer'Image( Board_Width ) &" "&
      Integer'Image( Board_Height )      
    );


  -- init tarvelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Init( I, Seeds(I+1), Symbol );   -- `Seeds(I+1)` is ugly :-(
    Symbol := Character'Succ( Symbol );
  end loop;

  -- start tarvelers tasks
  for I in Travel_Tasks'Range loop
    Travel_Tasks(I).Start;
  end loop;

end Travelers;
