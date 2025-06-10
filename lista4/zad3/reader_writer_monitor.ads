package Reader_Writer_Monitor is

   subtype Index is Natural range 1 .. 100;
   type Queue_Array is array(Index) of Character;

   task Monitor is
      entry Reader_Enter;
      entry Reader_Leave;
      entry Writer_Enter;
      entry Writer_Leave;
   end Monitor;

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

end Reader_Writer_Monitor;
