using System;
using System.Linq;


public class ListaLeniwa {
    private int len = 0;
    private int [] elements;

    virtual public int element(int i) {
        if (this.len == 0) {
            this.elements = new int[i];
            this.len = i;
            return this.elements[i-1];
        }
        else if (i <= this.len) {
            return this.elements[i-1];
        }
        else if (i > this.len) {
            int [] new_elements = new int[i - this.len];
            elements = elements.Concat(new_elements).ToArray();
            this.len = i;
            return this.elements[i-1];
        }
        return 1;
    }

    public int size() {
        return this.len;
    }
}


public class Pierwsze: ListaLeniwa {
    protected bool isPrime(int n) {
        for (int i=2; i<Math.Sqrt(n)+1; i++) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }

    protected int getPrime(int k) {
        if (k == 1) return 2;
        int j = 0;
        for (int i = 3; ; i+=2) {
            if (isPrime(i)) {
                j += 1;
                if (j == k) return i;
            }
        }
    }

    override public int element(int i) {
        int prime = this.getPrime(i);
        return base.element(prime);
    }
}


public class Program {
    public static void Main(string[] args) {
        Console.WriteLine("\nTesting ListaLeniwa: \n");

        ListaLeniwa lista;
        lista = new ListaLeniwa();
        Console.WriteLine(lista.size());
        Console.WriteLine("Add 10 elements");
        Console.WriteLine(lista.element(10));
        Console.WriteLine("Size: " + lista.size());

        Console.WriteLine("Add 10 more elements");
        Console.WriteLine(lista.element(20));
        Console.WriteLine("Size: " + lista.size());

        Console.WriteLine("Get 11th element");
        Console.WriteLine(lista.element(11));
        Console.WriteLine("Size: " + lista.size());

        Console.WriteLine("\nTesting Pierwsze: \n");

        Pierwsze pLista;
        pLista = new Pierwsze();
        Console.WriteLine(pLista.size());
        Console.WriteLine("Add 10th prime elements (" + pLista.getPrime(11) + ")");
        Console.WriteLine(pLista.element(10));
        Console.WriteLine("Size: " + pLista.size());

        Console.WriteLine("Add 10th prime more elements (" + pLista.getPrime(11) + ")");
        Console.WriteLine(pLista.element(20));
        Console.WriteLine("Size: " + pLista.size());

        Console.WriteLine("Get 11th prime element (" + pLista.getPrime(11) + ")");
        Console.WriteLine(pLista.element(11));
        Console.WriteLine("Size: " + pLista.size());
    }
}
