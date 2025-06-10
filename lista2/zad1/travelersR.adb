with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Real_Time;             use Ada.Real_Time;

with Ada.Wide_Characters.Handling;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Random_Seeds; use Random_Seeds;
   
procedure Travelers is

   Nr_Of_Travelers : constant Integer := 15;
   Nr_Of_Wild_Tenants : constant Integer := 10;
   Nr_Of_All        : constant Integer := Nr_Of_Travelers + Nr_Of_Wild_Tenants;

   Min_Steps       : constant Integer := 10;
   Max_Steps       : constant Integer := 100;

   Min_Delay       : constant Duration := 0.01;
   Max_Delay       : constant Duration := 0.05;

   Board_Width     : constant Integer := 15;
   Board_Height    : constant Integer := 15;

   Start_Time      : Time := Clock;

   Seeds           : Seed_Array_Type (1 .. Nr_Of_All) := Make_Seeds (Nr_Of_All);

   type Position_Type is record
      X : Integer range 0 .. Board_Width;
      Y : Integer range 0 .. Board_Height;
   end record;

      procedure Move_Down (Position : in out Position_Type) is
   begin
      Position.Y := (Position.Y + 1) mod Board_Height;
   end Move_Down;

   procedure Move_Up (Position : in out Position_Type) is
   begin
      Position.Y := (Position.Y + Board_Height - 1) mod Board_Height;
   end Move_Up;

   procedure Move_Right (Position : in out Position_Type) is
   begin
      Position.X := (Position.X + 1) mod Board_Width;
   end Move_Right;

   procedure Move_Left (Position : in out Position_Type) is
   begin
      Position.X := (Position.X + Board_Width - 1) mod Board_Width;
   end Move_Left;

   procedure Move_Direction (Position : in out Position_Type; Direction : Integer) is
   begin
      case Direction is
         when 0 =>
            Move_Up (Position);

         when 1 =>
            Move_Down (Position);

         when 2 =>
            Move_Left (Position);

         when 3 =>
            Move_Right (Position);

         when others =>
            Put_Line( " ?????????????? " & Integer'Image( Direction ) );
      end case;
   end Move_Direction;


 type Trace_Type is record
      Time_Stamp : Duration;
      Id         : Integer;
      Position   : Position_Type;
      Symbol     : Character;
   end record;

  type Trace_Array_type is array(0 .. Max_Steps * 2) of Trace_Type;

   type Traces_Sequence_Type is record
      Last        : Integer := -1;
      Trace_Array: Trace_Array_type; 
   end record;

   procedure Print_Trace( Trace : Trace_Type ) is
    Symbol : String := ( ' ', Trace.Symbol );
  begin
    Put_Line(
        Duration'Image( Trace.Time_Stamp ) & " " &
        Integer'Image( Trace.Id ) & " " &
        Integer'Image( Trace.Position.X ) & " " &
        Integer'Image( Trace.Position.Y ) & " " &
        ( ' ', Trace.Symbol ) 
      );
  end Print_Trace;

  procedure Print_Traces(Traces : Traces_Sequence_Type) is
  begin
    for I in 0 .. Traces.Last loop
      Print_Trace(Traces.Trace_Array(I));
    end loop;
  end Print_Traces;

      task Printer is
      entry Report (Traces : Traces_Sequence_Type);
   end Printer;

    task body Printer is
   begin
      for I in 1 .. Nr_Of_All loop
         accept Report (Traces : Traces_Sequence_Type) do
            Print_Traces (Traces);
         end Report;
      end loop;
   end Printer;


   type Traveler_Variant is (Normal_Traveler, Wild_Tenant, None);

   type Traveler_Type is record
      Id       : Integer;
      Symbol   : Character;
      Position : Position_Type;
   end record;

   task type Traveler_Task_Type is
      entry Init (Id : Integer; Seed : Integer; Symbol : Character);
      entry Start;
   end Traveler_Task_Type;

   task type Wild_Tenant_Task_Type is
      entry Init (Id : Integer; Seed : Integer; Symbol : Character);
      entry Start;
      entry Move_Out (New_Position : Position_Type);
   end Wild_Tenant_Task_Type;

   type Travelers_Task_Type (Variant : Traveler_Variant) is record
      case Variant is
         when Normal_Traveler =>
            Traveler_Task : Traveler_Task_Type;
         when Wild_Tenant =>
            Wild_Tenant_Task : Wild_Tenant_Task_Type;
         when None =>
            null; 
      end case;
   end record;

   protected type Field is
      entry Init (New_Position : Position_Type);
      entry Enter(New_Traveler : access Travelers_Task_Type; Success : out Boolean);
      entry Move (New_Position : Position_Type; Success : out Boolean);
      entry Leave;
   private
      Initialized : Boolean := False;
      Traveler    : access Travelers_Task_Type;
      Position    : Position_Type;
   end Field;

   Board: array (0 .. Board_Width - 1, 0 .. Board_Height - 1) of Field;
   Travel_Tasks : array (0 .. Nr_Of_All - 1) of access Travelers_Task_Type;
   Null_Task    : access Travelers_Task_Type :=
     new Travelers_Task_Type (Variant => None);

   protected body Field is
      entry Init (New_Position : Position_Type) when not Initialized is
      begin
         Position := New_Position;
         Traveler := Null_Task;
         Initialized := True;
      end Init;

      entry Enter
        (New_Traveler : access Travelers_Task_Type; Success : out Boolean)
        when Initialized and Traveler.Variant /= Normal_Traveler
      is
      begin
         if Traveler.Variant = None then
            Traveler := New_Traveler;
            Success := True;
         elsif Traveler.Variant = Wild_Tenant and New_Traveler.Variant = Normal_Traveler
         then
            declare
               New_Position : Position_Type;
            begin
               for N in 0 .. 3 loop
                  New_Position := Position;
                  Move_Direction (New_Position, N);
                  select
                     Board (New_Position.X, New_Position.Y).Enter
                       (Traveler, Success);
                     if Success then
                        exit;
                     end if;
                  else
                     Success := False;
                  end select;
               end loop;

               if Success then
                  Traveler.Wild_Tenant_Task.Move_Out (New_Position);
                  Traveler := New_Traveler;
               end if;
            end;
         else
            Success := False;
         end if;
      end Enter;

      entry Move (New_Position : Position_Type; Success : out Boolean)
        when Initialized and Traveler.Variant = Normal_Traveler
      is
      begin
         Board (New_Position.X, New_Position.Y).Enter (Traveler, Success);

         if Success then
            Traveler := Null_Task;
         end if;
      end Move;

      entry Leave when Initialized is
      begin
         Traveler := Null_Task;
      end Leave;
   end Field;

   task body Traveler_Task_Type is
      G           : Generator;
      Traveler    : Traveler_Type;
      Time_Stamp  : Duration;
      Nr_of_Steps : Integer;
      Traces      : Traces_Sequence_Type;

      procedure Store_Trace is
      begin
         Traces.Last := Traces.Last + 1;
         Traces.Trace_Array (Traces.Last) :=
           (Time_Stamp => Time_Stamp,
            Id         => Traveler.Id,
            Position   => Traveler.Position,
            Symbol     => Traveler.Symbol);
      end Store_Trace;

      procedure Make_Step (Position : in out Position_Type) is
         N : Integer;
      begin
         N := Integer (Float'Floor (4.0 * Random (G)));
         Move_Direction (Position, N);
      end Make_Step;

      New_Position : Position_Type;
      Success      : Boolean;
      Deadlock     : Boolean;

   begin
      accept Init (Id : Integer; Seed : Integer; Symbol : Character) do
         Reset (G, Seed);
         Nr_of_Steps :=
           Min_Steps + Integer (Float (Max_Steps - Min_Steps) * Random (G));
         Traveler.Id := Id;
         Traveler.Symbol := Symbol;

         Success := False;
         while not Success loop
            Traveler.Position :=
              (X => Integer (Float'Floor (Float (Board_Width) * Random (G))),
               Y => Integer (Float'Floor (Float (Board_Height) * Random (G))));
            select
               Board (Traveler.Position.X, Traveler.Position.Y).Enter
                 (Travel_Tasks (Traveler.Id), Success);
            else
               null;
            end select;
         end loop;
         Time_Stamp := To_Duration (Clock - Start_Time);
         Store_Trace;
      end Init;

      accept Start do
         null;
      end Start;

      Deadlock := False;
      for Step in 0 .. Nr_of_Steps loop
         delay Min_Delay + (Max_Delay - Min_Delay) * Duration (Random (G));
         Success := False;
         Deadlock := False;
         while not Success loop
            New_Position := Traveler.Position;
            Make_Step (New_Position);
            select
               Board (New_Position.X, New_Position.Y).Enter
                 (Travel_Tasks (Traveler.Id), Success);
            or
               delay 5 * Max_Delay;
               Deadlock := True;
               exit;
            end select;
         end loop;

         if Deadlock then
            Traveler.Symbol := Character'Val(Character'Pos(Traveler.Symbol) + 32); 
            Time_Stamp := To_Duration (Clock - Start_Time);
            Store_Trace;
            exit;
         else
            Board (Traveler.Position.X, Traveler.Position.Y).Leave;
            Traveler.Position := New_Position;
            Time_Stamp := To_Duration (Clock - Start_Time);
            Store_Trace;
         end if;
      end loop;

      Printer.Report (Traces);
   end Traveler_Task_Type;

   task body Wild_Tenant_Task_Type is
      G              : Generator;
      Traveler       : Traveler_Type;
      Time_Stamp     : Duration;
      Traces         : Traces_Sequence_Type;
      Time_Begin    : Duration;
      Time_End : Duration;

      procedure Store_Trace is
      begin
         Traces.Last := Traces.Last + 1;
         Traces.Trace_Array (Traces.Last) :=
           (Time_Stamp => Time_Stamp,
            Id         => Traveler.Id,
            Position   => Traveler.Position,
            Symbol     => Traveler.Symbol);
      end Store_Trace;

      Success : Boolean;
   begin
      accept Init (Id : Integer; Seed : Integer; Symbol : Character) do
         Reset (G, Seed);
         Traveler.Id := Id;
         Traveler.Symbol := Symbol;
         Time_Begin := 5 * Max_Delay * Duration (Random (G));
         Time_End := Time_Begin + (Max_Delay * Max_Steps - Time_Begin) * Duration (Random (G));
      end Init;

      accept Start do
         null;
      end Start;

      delay Time_Begin;
      Success := False;

      while not Success loop
         Traveler.Position :=
           (X => Integer (Float'Floor (Float (Board_Width) * Random (G))),
            Y => Integer (Float'Floor (Float (Board_Height) * Random (G))));
         select
            Board (Traveler.Position.X, Traveler.Position.Y).Enter
              (Travel_Tasks (Traveler.Id), Success);
         else
            null;
         end select;
      end loop;

      Time_Stamp := To_Duration (Clock - Start_Time);
      Store_Trace;

      loop
         select
            accept Move_Out (New_Position : Position_Type) do
               Traveler.Position := New_Position;
               Time_Stamp := To_Duration (Clock - Start_Time);
               Store_Trace;
            end Move_Out;
         or
            delay Max_Delay;
            if To_Duration (Clock - Start_Time) >= Time_End then
               Board (Traveler.Position.X, Traveler.Position.Y).Leave;
               Traveler.Position := (X => Board_Width, Y => Board_Height);
               Time_Stamp := To_Duration (Clock - Start_Time);
               Store_Trace;
               exit;
            end if;
         end select;
      end loop;

      Printer.Report (Traces);
   end Wild_Tenant_Task_Type;

   Symbol : Character;
   Id     : Integer;

   procedure Initialize_Board is
   begin
      for I in 0 .. Board_Width - 1 loop
         for J in 0 .. Board_Height - 1 loop
            Board(I, J).Init((X => I, Y => J));
         end loop;
      end loop;
   end Initialize_Board;

   procedure Initialize_Travelers is
   begin
      Symbol := 'A';
      for I in 0 .. Nr_Of_Travelers - 1 loop
         Travel_Tasks(Id) := new Travelers_Task_Type(Variant => Normal_Traveler);
         Travel_Tasks(Id).Traveler_Task.Init(Id, Seeds(Id + 1), Symbol);
         Symbol := Character'Succ(Symbol);
         Id := Id + 1;
      end loop;
   end Initialize_Travelers;

   procedure Initialize_Wild_Tenants is
   begin
      Symbol := '0';
      for I in 0 .. Nr_Of_Wild_Tenants - 1 loop
         Travel_Tasks(Id) := new Travelers_Task_Type(Variant => Wild_Tenant);
         Travel_Tasks(Id).Wild_Tenant_Task.Init(Id, Seeds(Id + 1), Symbol);
         Symbol := Character'Succ(Symbol);
         Id := Id + 1;
      end loop;
   end Initialize_Wild_Tenants;


   procedure Start_Travelers is
   begin
      for I in 0 .. Nr_Of_Travelers - 1 loop
         Travel_Tasks(I).Traveler_Task.Start;
      end loop;
   end Start_Travelers;

   procedure Start_Wild_Tenants is
   begin
      for I in Nr_Of_Travelers .. Nr_Of_All - 1 loop
         Travel_Tasks(I).Wild_Tenant_Task.Start;
      end loop;
   end Start_Wild_Tenants;

begin

   Put_Line(
      "-1 "
      & Integer'Image(Nr_Of_All)
      & " "
      & Integer'Image(Board_Width)
      & " "
      & Integer'Image(Board_Height)
   );


   Initialize_Board;
   Id := 0;
   Initialize_Travelers;
   Initialize_Wild_Tenants;

   Start_Travelers;
   Start_Wild_Tenants;
end Travelers;