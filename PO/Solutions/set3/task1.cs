using System;


public class Elem<T> {
    public T val;
    public Elem<T> next;
    public Elem<T> prev;

    public Elem(T val) {
        this.val = val;
    }
}


public class Lista<T> {
    protected Elem<T> first;
    protected Elem<T> last;
    protected int size = 0;

    public bool isEmpty() {
        return this.size == 0;
    }

    public int getSize() {
        return this.size;
    }

    public void pushFront(T val) {
        Elem<T> newElem = new Elem<T>(val);
        if (this.size == 0) {
            this.first = newElem;
            this.last = newElem;
        } else {
            newElem.next = this.first;
            this.first.prev = newElem;
            this.first = newElem;
        }
        this.size++;
    }

    public void pushBack(T val) {
        Elem<T> newElem = new Elem<T>(val);
        if (this.size == 0) {
            this.first = newElem;
            this.last = newElem;
        } else {
            this.last.next = newElem;
            newElem.prev = this.last;
            this.last = newElem;
        }
        this.size++;
    }

    public void push(T val) {
        this.pushBack(val);
    }

    public T popFirst() {
        if (this.isEmpty())
            throw new System.Exception("List is empty");
        Elem<T> top = this.first;

        this.first = this.first.next; 
        if (this.first != null)
            this.first.prev = null;

        this.size--;
        return top.val;
    }

    public T popLast() {
        if (this.isEmpty())
            throw new System.Exception("List is empty");
        T val = this.last.val;
        this.last = this.last.prev;

        if (this.last != null)
            this.last.next = null;

        this.size--;
        return val;
    }

    public T pop() {
        return this.popLast();
    }
}


public class Program {
    public static void Main(string[] args) {
        Console.WriteLine("Lista: \n");
        Lista<int> lista = new Lista<int>();
        Console.WriteLine("stacking 1, 2, 3, 4, -1: \n");
        lista.pushFront(1);
        lista.pushFront(2);
        lista.pushFront(3);
        lista.pushFront(4);
        lista.pushBack(-1);
        Console.WriteLine("size: ");
        Console.WriteLine(lista.getSize());
        Console.WriteLine("popping: \n");
        Console.WriteLine(lista.popFirst());
        Console.WriteLine(lista.popFirst());
        Console.WriteLine(lista.popFirst());
        Console.WriteLine(lista.popFirst());
        Console.WriteLine(lista.popFirst());
        Console.WriteLine("\n-----------------\n\n");

        Console.WriteLine("stacking 3 1 2 4: \n");
        lista.pushFront(1);
        lista.pushBack(2);
        lista.pushFront(3);
        lista.pushBack(4);
        Console.WriteLine("popping: \n");
        Console.WriteLine(lista.popLast());
        Console.WriteLine(lista.popLast());
        Console.WriteLine(lista.popLast());
        Console.WriteLine(lista.popLast());

    }
}
