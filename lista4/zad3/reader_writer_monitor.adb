with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
package body Reader_Writer_Monitor is

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
          -- shift everything one place left
          for J in I .. Last - 2 loop
            Queue(J) := Queue(J + 1);
          end loop;
          Last := Last - 1;
          exit;
        end if;
      end loop;
    end Remove_First_Of;

  end Request_Queue;

  task body Monitor is
    Readers : Natural := 0;
    Writers : Natural := 0;
    Waiting_Writers : Natural := 0;

  begin
    loop
      select
        accept Reader_Enter;
        Request_Queue.Enqueue('R');
        loop
          exit when Writers = 0 and Waiting_Writers = 0 and Request_Queue.Front = 'R';
          delay 0.001; -- polling-like wait
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
      or
        accept Reader_Leave;
        Readers := Readers - 1;
      or
        accept Writer_Leave;
        Writers := Writers - 1;
      end select;
    end loop;
  end Monitor;

end Reader_Writer_Monitor;
