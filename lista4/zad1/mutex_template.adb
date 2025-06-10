-- Improved and corrected version of Mutex_Template with proper Ada syntax
-- Implementing Szymański's mutual exclusion algorithm

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Random_Seeds;        use Random_Seeds;
with Ada.Real_Time;       use Ada.Real_Time;
with System.Atomic_Operations;

procedure Mutex_Template is

   Nr_Of_Processes : constant Integer := 15;
   subtype Flag_Value is Integer range 0 .. 4;

   type Flag_Value_Boolean_Array is array (Flag_Value) of Boolean;


   type Choosing_Array is array (0 .. Nr_Of_Processes - 1) of Boolean with Atomic_Components;
   type Number_Array   is array (0 .. Nr_Of_Processes - 1) of Integer with Atomic_Components;
   type Flag_Array     is array (1 .. Nr_Of_Processes) of Flag_Value with Atomic_Components;

   Flags : Flag_Array := (others => 0);

   MaxLabel : Integer := 0 with Atomic;
   Min_Steps : constant Integer := 50;
   Max_Steps : constant Integer := 100;

   Min_Delay : constant Duration := 0.01;
   Max_Delay : constant Duration := 0.05;

   -- States
   type Process_State is (Local_Section, Entry_Protocol_1, Entry_Protocol_2, Entry_Protocol_3, Entry_Protocol_4, Critical_Section, Exit_Protocol);
   Board_Width  : constant Integer := Nr_Of_Processes;
   Board_Height : constant Integer := Process_State'Pos(Process_State'Last) + 1;

   Start_Time : Time := Clock;

   Seeds : Seed_Array_Type(1 .. Nr_Of_Processes) := Make_Seeds(Nr_Of_Processes);

   type Position_Type is record
      X : Integer range 0 .. Board_Width - 1;
      Y : Integer range 0 .. Board_Height - 1;
   end record;

   type Trace_Type is record
      Time_Stamp : Duration;
      Id         : Integer;
      Position   : Position_Type;
      Symbol     : Character;
   end record;

   type Trace_Array_Type is array (0 .. 10   *Max_Steps) of Trace_Type;

   type Traces_Sequence_Type is record
      Last        : Integer := -1;
      Trace_Array : Trace_Array_Type;
   end record;

   procedure Print_Trace(Trace : Trace_Type) is
   begin
      Put_Line(Duration'Image(Trace.Time_Stamp) & " " &
               Integer'Image(Trace.Id-1) & " " &
               Integer'Image(Trace.Position.X) & " " &
               Integer'Image(Trace.Position.Y) & " " &
               Trace.Symbol);
   end Print_Trace;

   procedure Print_Traces(Traces : Traces_Sequence_Type) is
   begin
      for I in 0 .. Traces.Last loop
         Print_Trace(Traces.Trace_Array(I));
      end loop;
   end Print_Traces;

   task Printer is
      entry Report(Traces : Traces_Sequence_Type);
   end Printer;

   task body Printer is
   begin
      for I in 1 .. Nr_Of_Processes loop
         accept Report(Traces : Traces_Sequence_Type) do
            Print_Traces(Traces);
         end Report;
      end loop;

      Put("-1 " & Integer'Image(Nr_Of_Processes) & " " &
                  Integer'Image(Board_Width) & " " &
                  Integer'Image(Board_Height) & " ");
      for I in Process_State'Range loop
         Put(Process_State'Image(I) & ";");
      end loop;
   end Printer;

   type Process_Type is record
      Id       : Integer;
      Symbol   : Character;
      Position : Position_Type;
   end record;

   task type Process_Task_Type is
      entry Init(Id : Integer; Seed : Integer; Symbol : Character);
      entry Start;
   end Process_Task_Type;

   task body Process_Task_Type is
      G : Generator;
      P : Process_Type;
      Time_Stamp : Duration;
      Steps : Integer;
      Traces : Traces_Sequence_Type;

      procedure Rand_Delay is
      begin
         delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
      end Rand_Delay;

      procedure Set_Flag(ID : Integer; Value : Flag_Value) is
      begin
         Flags(ID) := Value;
      end Set_Flag;

      function Snapshot return Flag_Array is
         Copy : Flag_Array;
      begin
         for I in Copy'Range loop
            Copy(I) := Flags(I);
         end loop;
         return Copy;
      end Snapshot;

      function All_In(States : Flag_Array; Allowed : Flag_Value_Boolean_Array) return Boolean is
      begin
         for I in States'Range loop
            if not Allowed(States(I)) then
               return False;
            end if;
         end loop;
         return True;
      end All_In;

      function Any_In(States : Flag_Array; Wanted : Flag_Value) return Boolean is
      begin
         for I in States'Range loop
            if States(I) = Wanted then
               return True;
            end if;
         end loop;
         return False;
      end Any_In;

      procedure Store_Trace is
      begin
         Traces.Last := Traces.Last + 1;
         Traces.Trace_Array(Traces.Last) := (
            Time_Stamp => Time_Stamp,
            Id => P.Id,
            Position => P.Position,
            Symbol => P.Symbol);
      end Store_Trace;

      procedure Change_State(State : Process_State) is
      begin
         Time_Stamp := To_Duration(Clock - Start_Time);
         P.Position.Y := Process_State'Pos(State);
         Store_Trace;
      end Change_State;

   begin
      accept Init(Id : Integer; Seed : Integer; Symbol : Character) do
         Reset(G, Seed);
         P.Id := Id + 1; -- ID from 1 to Nr_Of_Processes
         P.Symbol := Symbol;
         P.Position := (X => Id, Y => Process_State'Pos(Local_Section));
         Steps := Min_Steps + Integer(Float(Max_Steps - Min_Steps) * Random(G));
         Time_Stamp := To_Duration(Clock - Start_Time);
         Store_Trace;
      end Init;

      accept Start do
         null;
      end Start;

      for I in 1 .. Steps loop
         --Put_Line (Integer'Image(I) & " " & Integer'Image(Steps));
         Rand_Delay;

         -- Etap 1
         Set_Flag(P.Id, 1);
         Change_State(Entry_Protocol_1);

         -- Etap 2
         loop
            declare
               Snap : constant Flag_Array := Snapshot;
               Allowed : constant Flag_Value_Boolean_Array := (0 => True, 1 => True, 2 => True, others => False);
            begin
               exit when All_In(Snap, Allowed);
               delay 0.001;
            end;
         end loop;

         Set_Flag(P.Id, 3);
         Change_State(Entry_Protocol_2);

         -- Etap 3
         declare
            Snap : Flag_Array := Snapshot;
            Waiting : Boolean := False;
         begin
            for J in Snap'Range loop
               if J /= P.Id and then Snap(J) = 1 then
                  Waiting := True;
                  exit;
               end if;
            end loop;

            if Waiting then
               Set_Flag(P.Id, 2);
               Change_State(Entry_Protocol_3);

               loop
                  Snap := Snapshot;
                  exit when Any_In(Snap, 4);
                  delay 0.001;
               end loop;
            end if;
         end;

         Set_Flag(P.Id, 4);
         Change_State(Entry_Protocol_4);

         loop
            declare
               Snap : constant Flag_Array := Snapshot;
               Ok : Boolean := True;
            begin
               for J in 1 .. P.Id - 1 loop
                  if Snap(J) /= 0 and then Snap(J) /= 1 then
                     Ok := False;
                     exit;
                  end if;
               end loop;
               exit when Ok;
               delay 0.001;
            end;
         end loop;

         Change_State(Critical_Section);
         Rand_Delay;

         Change_State(Exit_Protocol);

         loop
            declare
               Snap : constant Flag_Array := Snapshot;
               Ok : Boolean := True;
            begin
               for J in P.Id + 1 .. Nr_Of_Processes loop
                  if Snap(J) /= 0 and then Snap(J) /= 1 and then Snap(J) /= 4 then
                     Ok := False;
                     exit;
                  end if;
               end loop;
               exit when Ok;
               delay 0.001;
            end;
         end loop;

         Set_Flag(P.Id, 0);
         Change_State(Local_Section);
      end loop;

      if MaxLabel < Steps then
         MaxLabel := Steps;
      end if;

      Printer.Report(Traces);
   end Process_Task_Type;

   Process_Tasks : array (0 .. Nr_Of_Processes - 1) of Process_Task_Type;
   Symbol : Character := 'A';

begin
   for I in Process_Tasks'Range loop
      Process_Tasks(I).Init(I, Seeds(I + 1), Symbol);
      Symbol := Character'Succ(Symbol);
   end loop;

   for I in Process_Tasks'Range loop
      Process_Tasks(I).Start;
   end loop;
end Mutex_Template;