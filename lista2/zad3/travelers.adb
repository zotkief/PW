with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Seeds; use Random_Seeds;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Characters.Handling; use Ada.Characters.Handling;

procedure Travelers is

   completedTravel : Integer :=0;
   closing : Boolean := false;
   ghostCount : Integer :=10;
   -- Travelers moving on the board
   Nr_Of_Travelers : constant Integer := 15;
   Min_Steps : constant Integer := 10;
   Max_Steps : constant Integer := 100;

   GhostId : Integer := 2*Nr_Of_Travelers;

   Min_Delay : constant Duration := 0.01;
   Max_Delay : constant Duration := 0.05;

   -- 2D Board with torus topology
   Board_Width  : constant Integer := 15;
   Board_Height : constant Integer := 15;

   -- Timing
   Start_Time : Time := Clock;  -- global starting time

   -- Random seeds for the tasks' random number generators
   Seeds : Seed_Array_Type(1..Nr_Of_Travelers) := Make_Seeds(Nr_Of_Travelers);

   type Boolean_Board_Type is array (0 .. Board_Width - 1, 0 .. Board_Height - 1) of Boolean;
   traps : Boolean_Board_Type;

   -- Types, procedures and functions
   -- Postitions on the board
   type Position_Type is record  
      X: Integer range 0 .. Board_Width; 
      Y: Integer range 0 .. Board_Height; 
   end record;

   task type Ghost_Task_Type is
      entry Init(Pos : Position_Type; Delay_Time : Duration);
      entry Update_Position(Pos : Position_Type) when not working;
   private
      working:Boolean:=false;
   end Ghost_Task_Type;


   type Ghost_Access is access all Ghost_Task_Type;
   Ghosts : array (0 .. Board_Width-1, 0 .. Board_Height-1) of Ghost_Access := (others => (others => null));
   -- Task type for a cell (representing each board cell)
   protected type Cell_Task_Type is
      entry Request_Enter(Resp : out Boolean; Traveler_Id : Integer);
      entry Leave;
      entry Request_Ghost_Enter(Resp : out Boolean;Id : in Integer);
      entry Init(Position : Position_Type);
   private
      Occupied_By_Traveler : Boolean := False;
      Occupied_By_Ghost    : Boolean := False;
      Ghost_Kick_Requested : Boolean := False;
      Initialized : Boolean:=false;
      G:Generator;
      pos : Position_Type;
      localResp : Boolean;
      found : Boolean;
      Dir : Integer;
      Local_OK: Boolean := False;
   end Cell_Task_Type;

   -- Board: 2D array of Cell_Task_Type tasks
   Board : array (0 .. Board_Width-1, 0 .. Board_Height-1) of Cell_Task_Type;

   -- Body of the task for a cell
   protected body Cell_Task_Type is
      entry Init(Position : Position_Type) when not Initialized is
      begin
         pos := Position;
         Initialized:=true; 
      end Init;


      entry Request_Enter(Resp : out Boolean; Traveler_Id : Integer) when Initialized is 
      begin
         --Put_Line ("id:" & Integer'Image(Traveler_Id) & "ghost:" & Boolean'Image(Occupied_By_Ghost)& "trav:" & Boolean'Image(Occupied_By_Traveler));
         -- 1) tylko ustaw flagi
         --Put_Line (Integer'Image(Traveler_Id) & " przyjąłem");
         if not Occupied_By_Traveler and not Occupied_By_Ghost then
            Resp := True;
            Occupied_By_Traveler := True;
         elsif not Occupied_By_Traveler and Occupied_By_Ghost then
            --Put_Line (Integer'Image(Traveler_Id) & " duch");
            Resp:=false;
            --=======================
            Dir:=1;
            Local_OK := False;
            while Dir<5 and not Local_OK loop

               --Put_Line (Integer'Image(Traveler_Id) & " pętla");
               declare
                  New_Pos : Position_Type;
               begin
                  if Dir = 1 then
                     New_Pos := (X => (pos.X+1) mod Board_Width, Y => pos.Y);
                  elsif Dir = 2 then
                     New_Pos := (X => (pos.X-1+Board_Width) mod Board_Width, Y => pos.Y);
                  elsif Dir = 3 then
                     New_Pos := (X => pos.X, Y => (pos.Y+1) mod Board_Height);
                  elsif Dir = 4 then
                     New_Pos := (X => pos.X, Y => (pos.Y-1+Board_Height) mod Board_Height);
                  end if;

                  
                  Local_OK:=false;
                  select
                     Board(New_Pos.X, New_Pos.Y).Request_Ghost_Enter(Local_OK,Traveler_Id);
                  else
                     null;
                  end select;


                  --Put_Line (Integer'Image(Traveler_Id) & " s" & Boolean'Image(Local_OK));

                  if Local_OK=true then
            --Put_Line (Integer'Image(Traveler_Id) & " dalej1.5");
                     Resp:=true;
                     Occupied_By_Ghost := False;
                     select
                        Ghosts(pos.X, pos.Y).Update_Position(New_Pos);
                     else
                        null;
                     end select;
                     Ghosts(New_Pos.X, New_Pos.Y) := Ghosts(pos.X, pos.Y);
                     Occupied_By_Traveler := True;

            --Put_Line (Integer'Image(Traveler_Id) & " dalej1");
                  end if;
                  --Put_Line ("id:" & Integer'Image(Traveler_Id) & "gresp:" & Boolean'Image(Resp)& ":" & Boolean'Image(Occupied_By_Traveler));

               end;
               Dir:=Dir+1;
            --Put_Line (Integer'Image(Traveler_Id) & " dalej2");
            end loop;
            --=======================
            --Put_Line (Integer'Image(Traveler_Id) & " dalej3");
         else
            Resp := False;
            --Put_Line (Integer'Image(Traveler_Id) & " dalej4");
         end if;
         --Put_Line ("id:" & Integer'Image(Traveler_Id) & "gresp:" & Boolean'Image(Resp)& "niewiem:" & Boolean'Image(Occupied_By_Traveler));

            --Put_Line (Integer'Image(Traveler_Id) & " dalej5");
      end Request_Enter;

      entry Request_Ghost_Enter(Resp : out Boolean;Id : in Integer) when Initialized is
      begin
         --Put_Line (Integer'Image(Id) & " przyjąłem-duch");
         if not Occupied_By_Traveler and not Occupied_By_Ghost then
            Resp := True;
            Occupied_By_Ghost := True;

         --Put_Line (Integer'Image(Id) & " decyzja-duch+");
         else
            Resp := False;
         --Put_Line (Integer'Image(Id) & " decyzja-duch-");
         end if;
      end Request_Ghost_Enter;

      entry Leave when Initialized is
      begin
         Occupied_By_Traveler := False;
         Occupied_By_Ghost :=False;
         Ghosts(pos.X,pos.Y):=null;
      end Leave;
   end Cell_Task_Type;
   -- Elementary steps
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
      New_Position.X := ( Position.X - 1 + Board_Width ) mod Board_Width;
      return New_Position;
   end Move_Left;

   -- Traces of travelers
   type Trace_Type is record
      Time_Stamp: Duration;      
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

   -- Task Printer collects and prints reports of traces
   task Printer is
      entry Report( Traces : Traces_Sequence_Type );
   end Printer;
   
   task body Printer is
   begin
      loop
         accept Report( Traces : Traces_Sequence_Type ) do
            Print_Traces( Traces );
         end Report;
         exit when closing;  -- lub inny warunek zakoĹczenia
      end loop;
   end Printer;


   -- Travelers
   type Traveler_Type is record
      Id: Integer;
      Symbol: Character;
      Position: Position_Type;    
   end record;


   task body Ghost_Task_Type is
      Current : Position_Type;
      Max_Delay : constant Duration := 1.0;
      T0 : Time := Clock;
      Elapsed_Time : Duration;
      Traces : Traces_Sequence_Type;
      Symbol : Character;
      Id : Integer;
      Time_Stamp : Duration;
      Start_Delay : Duration;
      Time_Left : Duration := 3*Max_Delay;
      working:Boolean:=false;

      procedure Store_Trace is
      begin  
         Traces.Last := Traces.Last + 1;
         Traces.Trace_Array(Traces.Last) := (
            Time_Stamp => Time_Stamp,
            Id => Id,
            Position => Current,
            Symbol => Symbol
         );
      end Store_Trace;

   begin
      accept Init(Pos : Position_Type; Delay_Time : Duration) do
         Time_Stamp := To_Duration(Clock - Start_Time);
         Current := Pos;
         Symbol := '1';
         Id := GhostId;
         GhostId := GhostId + 1;
         Start_Delay := Delay_Time;
      end Init;

      delay Start_Delay;  -- duch czeka określony czas przed rozpoczęciem działania
      T0:=Clock;
      Store_Trace;

      loop
         select
            accept Update_Position(Pos : Position_Type) do
               Current := Pos;
               Time_Stamp := To_Duration(Clock - Start_Time);
               Store_Trace;
               Time_Left:=Time_Left-Ada.Real_Time.To_Duration(Clock-T0);


               if traps(Pos.X,Pos.Y) then
                  Symbol:='*';
                  Time_Stamp := To_Duration ( Clock - Start_Time );
                  Store_Trace;
                  delay Max_Delay;
                  Traveler.Symbol:='#';
                  id:=100+Pos.X*Board_Width+Pos.Y;
                  Store_Trace;
                  exit;
               end if;
            end Update_Position;
         or
            delay Time_Left;
            Time_Stamp := To_Duration(Clock - Start_Time);
            Board(Current.X,Current.Y).Leave;
            Current.X := 15;
            Current.Y := 15;
            Store_Trace;
            exit;
         end select;
      end loop;
      Printer.Report(Traces);
   end Ghost_Task_Type;

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
         continuer : Boolean := true;
         Start_Time : Time := Clock;
         Elapsed_Time : Duration;
         Accepted : Boolean;
         LocalTime : Time;
         Skipping : Boolean;
      begin 

         loop
            --Put_Line ("czekam");
            N := Integer( Float'Floor(4.0 * Random(G)) );    
            
            case N is
               when 0 =>
                  tempPos := Move_Up( Traveler.Position );
               when 1 =>
                  tempPos := Move_Down( Traveler.Position );
               when 2 =>
                  tempPos := Move_Left( Traveler.Position );
               when 3 =>
                  tempPos := Move_Right( Traveler.Position );
               when others =>
                  Put_Line( " ?????????????? " & Integer'Image( N ) );
            end case;


            Accepted := False;
            --Put_Line (Integer'Image(Traveler.Id) & " wchodze");
            select
               Board(tempPos.X, tempPos.Y).Request_Enter(Accepted, Traveler.Id);
            else
               null;
            end select;
            
            --Put_Line (Integer'Image(Traveler.Id) & " dalej");
            if Accepted then
               --Put_Line (Integer'Image(Traveler.Id) & " wychodze");
               Board(Traveler.Position.X, Traveler.Position.Y).Leave;
               -- If accepted, update the position and store the trace  
               Traveler.Position := tempPos;
               Time_Stamp := To_Duration ( Clock - Start_Time ); -- reads global clock
               Store_Trace;
               return true;
            else
               -- If not accepted, check the elapsed time
               Elapsed_Time := To_Duration(Clock - Start_Time);
               if Elapsed_Time > 3 * Max_Delay then
                  -- If elapsed time exceeds 3 * Max_Delay, return false
                  return false;
               end if;

               -- If not accepted, wait for a random time between Min_Delay and Max_Delay
               delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
            end if;
         end loop;
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

      for Step in 0 .. Nr_of_Steps loop
         delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
         accepter := Make_Step;


         if not accepter then
            Traveler.Symbol := Ada.Characters.Handling.To_Lower(Traveler.Symbol);
            Time_Stamp := To_Duration ( Clock - Start_Time );
            Store_Trace;
            exit;
         end if;


         if traps(Traveler.Position.X,Traveler.Position.Y) then
            Traveler.Symbol := Ada.Characters.Handling.To_Lower(Traveler.Symbol);
            Time_Stamp := To_Duration ( Clock - Start_Time );
            Store_Trace;
            delay Max_Delay;
            Traveler.Symbol:='#';
            Traveler.Id:=100+Traveler.Position.X*Board_Width+Traveler.Position.Y;
            Store_Trace;
            exit;
         end if;
      end loop;
      Printer.Report( Traces );
      completedTravel:=completedTravel+1;
      if completedTravel>=15 then
         closing:=true;
      end if;
   end Traveler_Task_Type;

   -- Local for main task
   Travel_Tasks: array (0 .. Nr_Of_Travelers - 1) of Traveler_Task_Type;
   Symbol : Character := 'A';
   procedure Spawn_Ghosts is
      Ghost_Generator : Generator;
      Pos_X, Pos_Y : Integer;
      Delay_Seconds : Duration;
      Accepter : Boolean := false;
   begin
      Reset(Ghost_Generator);
      for I in 1 .. ghostCount loop
         -- Wygeneruj losową pozycję
         Pos_X := Integer(Float'Floor(Float(Board_Width)  * Random(Ghost_Generator)));
         Pos_Y := Integer(Float'Floor(Float(Board_Height) * Random(Ghost_Generator)));

         -- Wygeneruj losowe opóźnienie od 0.01 do 2.0 sekundy
         Delay_Seconds := 0.01 + (2.0 - 0.01) * Duration(Random(Ghost_Generator));

         -- Utwórz ducha i zainicjalizuj go
         select
            Board(Pos_X,Pos_Y).Request_Ghost_Enter(Accepter,0);
         else
            delay 0.1;
            Accepter:=false;
         end select;
         if Accepter then
            Ghosts(Pos_X, Pos_Y) := new Ghost_Task_Type;
            Ghosts(Pos_X, Pos_Y).Init(
               Pos => (X => Pos_X, Y => Pos_Y),
               Delay_Time => Delay_Seconds
            );
         end if;
      end loop;
   end Spawn_Ghosts;
   G : Generator;
   R : Float;
begin 

   for X in 0 .. Board_Width-1 loop
      for Y in 0 .. Board_Height-1 loop
         R := Random(G);

         if R < 0.1 then
            traps(X, Y) := True;
         else
            traps(X, Y) := False;
         end if;
         Board(X, Y).Init(Position => (X => X, Y => Y));

      end loop;
   end loop;
   -- Print the line with the parameters needed for display script:
   Put_Line(
      "-1 " &
      Integer'Image( Nr_Of_Travelers ) & " " &
      Integer'Image( Board_Width ) & " " &
      Integer'Image( Board_Height )
   );

   -- Initialize traveler tasks
   for I in Travel_Tasks'Range loop
      Travel_Tasks(I).Init( I, Seeds(I+1), Symbol );   -- Seeds(I+1) is ugly :-(
      Symbol := Character'Succ( Symbol );
   end loop;

   -- Start traveler tasks
   for I in Travel_Tasks'Range loop
      Travel_Tasks(I).Start;
   end loop;

   Spawn_Ghosts;

end Travelers;