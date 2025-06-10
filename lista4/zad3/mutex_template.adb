with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Real_Time; use Ada.Real_Time;
with Random_Seeds; use Random_Seeds;

procedure Mutex_Template is
   -- ========================
   --  MONITOR I KOLEJKA ŻĄDAŃ
   -- ========================

   subtype Index is Natural range 1 .. 100;
   type Queue_Array is array(Index) of Character;
   Readers : Natural := 0;
   Writers : Natural := 0;

   procedure Reader_Leave is
   begin
      Readers := Readers - 1;
   end Reader_Leave;

   procedure Writer_Leave is
   begin
      Writers := Writers - 1;
   end Writer_Leave;



   protected Request_Queue is
      procedure Enqueue(S : in Character);
      function Front return Character;
      procedure Dequeue;
      function Is_Empty return Boolean;
      function Exists(S : Character) return Boolean;
      procedure Remove_First_Of(S : Character);
   private
      Queue : Queue_Array;
      First : Index := 1;
      Last  : Index := 1;
   end Request_Queue;

   protected body Request_Queue is
      procedure Enqueue(S : in Character) is
      begin
         Queue(Last) := S;
         Last := Last + 1;
      end Enqueue;

      function Front return Character is
      begin
         return Queue(First);
      end Front;

      procedure Dequeue is
      begin
         First := First + 1;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return First = Last;
      end Is_Empty;

      function Exists(S : Character) return Boolean is
      begin
         for I in First .. Last - 1 loop
            if Queue(I) = S then
               return True;
            end if;
         end loop;
         return False;
      end Exists;

      procedure Remove_First_Of(S : Character) is
      begin
         for I in First .. Last - 1 loop
            if Queue(I) = S then
               for J in I .. Last - 2 loop
                  Queue(J) := Queue(J + 1);
               end loop;
               Last := Last - 1;
               exit;
            end if;
         end loop;
      end Remove_First_Of;
   end Request_Queue;

   task Monitor is
      entry Reader_Enter;
      entry Writer_Enter;
   end Monitor;

   task body Monitor is
      Waiting_Writers : Natural := 0;
   begin
      loop
         select
            accept Reader_Enter;
            Request_Queue.Enqueue('R');
            loop
               exit when Writers = 0 and Waiting_Writers = 0 and Request_Queue.Front = 'R';
               delay 0.001;
            end loop;
            Readers := Readers + 1;
            Request_Queue.Remove_First_Of('R');
         or
            accept Writer_Enter;
            Request_Queue.Enqueue('W');
            Waiting_Writers := Waiting_Writers + 1;
            loop
               exit when Readers = 0 and Writers = 0 and Request_Queue.Front = 'W';
               delay 0.001;
            end loop;
            Waiting_Writers := Waiting_Writers - 1;
            Writers := Writers + 1;
            Request_Queue.Remove_First_Of('W');
         end select;
      end loop;
   end Monitor;

   -- ========================
   --  KONFIGURACJA SYMULACJI
   -- ========================
  Nr_Of_Readers    : constant Integer := 10;
  Nr_Of_Writers    : constant Integer := 5;
  Nr_Of_Processes  : constant Integer := Nr_Of_Readers + Nr_Of_Writers;

  Min_Steps : constant Integer := 50;
  Max_Steps : constant Integer := 100;

  Min_Delay : constant Duration := 0.01;
  Max_Delay : constant Duration := 0.05;

   procedure Start_Reading is
   begin
      Monitor.Reader_Enter;
   end Start_Reading;

   procedure Stop_Reading is
   begin
      Reader_Leave;
   end Stop_Reading;

   procedure Start_Writing is
   begin
      Monitor.Writer_Enter;
   end Start_Writing;

   procedure Stop_Writing is
   begin
      Writer_Leave;
   end Stop_Writing;

  type Process_State is (
    Local_Section,
    Start,
    Reading_Room,
    Stop
  );

  Board_Width  : constant Integer := Nr_Of_Processes;
  Board_Height : constant Integer := Process_State'Pos(Process_State'Last) + 1;

  Start_Time : Time := Clock;

  Seeds : Seed_Array_Type(1..Nr_Of_Processes) := Make_Seeds(Nr_Of_Processes);

  type Position_Type is record
    X: Integer range 0 .. Board_Width - 1;
    Y: Integer range 0 .. Board_Height - 1;
  end record;

  type Trace_Type is record
    Time_Stamp: Duration;
    Id : Integer;
    Position: Position_Type;
    Symbol: Character;
  end record;

  type Trace_Array_Type is array(0 .. Max_Steps) of Trace_Type;

  type Traces_Sequence_Type is record
    Last: Integer := -1;
    Trace_Array: Trace_Array_Type;
  end record;

  procedure Print_Trace(Trace: Trace_Type) is
  begin
    Put_Line(Duration'Image(Trace.Time_Stamp) & " " &
             Integer'Image(Trace.Id) & " " &
             Integer'Image(Trace.Position.X) & " " &
             Integer'Image(Trace.Position.Y) & " " &
             (' ', Trace.Symbol));
  end Print_Trace;

  procedure Print_Traces(Traces: Traces_Sequence_Type) is
  begin
    for I in 0 .. Traces.Last loop
      Print_Trace(Traces.Trace_Array(I));
    end loop;
  end Print_Traces;

  task Printer is
    entry Report(Traces: Traces_Sequence_Type);
  end Printer;

  task body Printer is
  begin
    for I in 1 .. Nr_Of_Processes loop
      accept Report(Traces: Traces_Sequence_Type) do
        Print_Traces(Traces);
      end Report;
    end loop;

    Put("-1 " & Integer'Image(Nr_Of_Processes) & " " &
        Integer'Image(Board_Width) & " " & Integer'Image(Board_Height) & " ");
    for I in Process_State'Range loop
      Put(I'Image & ";");
    end loop;
    Put_Line("EXTRA_LABEL;");
  end Printer;

  type Process_Type is record
    Id: Integer;
    Symbol: Character;
    Position: Position_Type;
  end record;

  task type Process_Task_Type is
    entry Init(Id: Integer; Seed: Integer; Symbol: Character);
    entry Start;
  end Process_Task_Type;

  task body Process_Task_Type is
    G: Generator;
    P: Process_Type;
    Time_Stamp: Duration;
    Steps: Integer;
    Traces: Traces_Sequence_Type;

    procedure Store_Trace is
    begin
      Traces.Last := Traces.Last + 1;
      Traces.Trace_Array(Traces.Last) := (
        Time_Stamp => Time_Stamp,
        Id => P.Id,
        Position => P.Position,
        Symbol => P.Symbol
      );
    end Store_Trace;

    procedure Change_State(State: Process_State) is
    begin
      Time_Stamp := To_Duration(Clock - Start_Time);
      P.Position.Y := Process_State'Pos(State);
      Store_Trace;
    end Change_State;

    Is_Reader: Boolean := False;

  begin
    accept Init(Id: Integer; Seed: Integer; Symbol: Character) do
      Reset(G, Seed);
      P.Id := Id;
      P.Symbol := Symbol;
      Is_Reader := (Symbol = 'R');
      P.Position := (X => Id, Y => Process_State'Pos(Local_Section));
      Steps := Min_Steps + Integer(Float(Max_Steps - Min_Steps) * Random(G));
      Time_Stamp := To_Duration(Clock - Start_Time);
      Store_Trace;
    end Init;

    accept Start;

    for I in 1 .. Steps / 4 loop
      Put_Line (Integer'Image(I) & " " & Integer'Image(Steps));
      Put_Line (Integer'Image(P.Id) & "p1");
      delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
      Change_State(Start);

      if Is_Reader then
         Put_Line (Integer'Image(P.Id) & "r2");
        Start_Reading;
         Put_Line (Integer'Image(P.Id) & "r3");
        Change_State(Reading_Room);
        delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
         Put_Line (Integer'Image(P.Id) & "r4");
        Stop_Reading;
      else
         Put_Line (Integer'Image(P.Id) & "w2");
        Start_Writing;
         Put_Line (Integer'Image(P.Id) & "w3");
        Change_State(Reading_Room);
        delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
         Put_Line (Integer'Image(P.Id) & "w4");
        Stop_Writing;
      end if;

      Change_State(Stop);
      Put_Line (Integer'Image(P.Id) & "p5");
      delay Min_Delay + (Max_Delay - Min_Delay) * Duration(Random(G));
      Change_State(Local_Section);
    end loop;

    Printer.Report(Traces);
  end Process_Task_Type;

  Process_Tasks: array(0 .. Nr_Of_Processes - 1) of Process_Task_Type;

  Symbol: Character := 'R';

begin
  for I in 0 .. Nr_Of_Readers - 1 loop
    Process_Tasks(I).Init(I, Seeds(I + 1), Symbol);
  end loop;

  Symbol := 'W';
  for I in Nr_Of_Readers .. Nr_Of_Processes - 1 loop
    Process_Tasks(I).Init(I, Seeds(I + 1), Symbol);
  end loop;

  for I in Process_Tasks'Range loop
    Process_Tasks(I).Start;
  end loop;

end Mutex_Template;
