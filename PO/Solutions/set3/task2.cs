using System;


public class HashNode<K, V> {
    public K key;
    public V val;
    public HashNode<K, V> next;

    public HashNode(K key, V val) {
        this.key = key;
        this.val = val;
    }

    public void setOrUpdate(K key, V val) {
        HashNode<K, V> head = this;
        while (head.next != null) {
            if (head.key.Equals(key)) {
                head.val = val;
                return;
            }
            head = head.next;
        }
    }
}


public class Slownik<K, V> {
    /* Hash Map implementation.

    Collisions are handled by separate chaining.

    Bucket (table) allocation is done dynamically,
    Each time load factor exceeds certain value,
    bucket capacity is doubled

    For each new object hash calculated.
    */
    protected HashNode<K, V>[] bucket;
    protected int capacity;
    protected int size;

    public void allocateNewBucket(int capacity) {
        if (this.bucket == null){
            this.bucket = new HashNode<K, V>[this.capacity];
            return;
        }
        HashNode<K, V>[] tempBucket = this.bucket;
        this.bucket = new HashNode<K, V>[this.capacity];

        // Copy elements to extended bucket
        for (int i=0; i<this.capacity; i++) {
            this.bucket[i] = tempBucket[i];
        }
        this.capacity = capacity;
    }

    public Slownik() {
        this.size = 0;
        this.capacity = 8;
        this.allocateNewBucket(this.capacity);
    }

    public void print() {
        Console.WriteLine("{ ");
        for (int i=0; i<this.capacity; i++) {
            if (this.bucket[i] != null) {
                Console.WriteLine(this.bucket[i].key + ": " +
                                  this.bucket[i].val + ", ");
            }
        }
        Console.WriteLine("}");
    }

    public bool isEmpty() {
        return this.size == 0;
    }

    public int getSize() {
        return this.size;
    }

    protected int getBucketIndex(K key) {
        long hashCode = key.GetHashCode() & ~(1 << 63);
        return (int)(hashCode % (this.capacity - 1));
    }

    protected double getLoadFactor() {
        return this.size * 1.0 / this.capacity;
    }

    protected void adjustCapacity() {
        // Increase size of the bucket if load factor is too big
        double loadFactor = this.getLoadFactor();
        if (loadFactor >= 0.7) {
            this.capacity *= 2;
            this.allocateNewBucket(this.capacity);
        }
    }

    public void create(K key, V val) {
        int index = this.getBucketIndex(key);
        HashNode<K, V> head = this.bucket[index];

        if (head == null) {
            this.bucket[index] = new HashNode<K, V>(key, val);
        } else {
            this.bucket[index].setOrUpdate(key, val);
        }

        this.size++;
        this.adjustCapacity();
    }

    public V get(K key) {
        int index = this.getBucketIndex(key);
        HashNode<K, V> head = this.bucket[index];
        
        // Search key in chain
        while (head != null) {
            if (head.key.Equals(key))
                return head.val;
            head = head.next;
        }
        throw new System.Exception("Key Error: Element not found");
    }

    public V remove(K key) {
        int index = this.getBucketIndex(key);
        HashNode<K, V> head = this.bucket[index];
        HashNode<K, V> prev = null;

        while (head != null) {
            if (head.key.Equals(key)) break;
            prev = head;
            head = head.next;
        }

        if (head == null)
            throw new System.Exception("Key Error: Element not found");
        if (prev != null)
            prev.next = head.next;
        else this.bucket[index] = head.next;
        this.size--;
        return head.val;
    }
}


public class Program {
    public static void Main(string[] args) {
        Console.WriteLine("Hash Map: \n");
        Slownik<string, int> slownik = new Slownik<string, int>();
        Console.WriteLine("Adding 3 elements... ");
        slownik.create("first", 17);
        slownik.create("second", 2);
        slownik.create("third", 42);
        Console.WriteLine("Created structure: ");
        slownik.print();

        Console.WriteLine("------");
        Console.WriteLine("Size: ");
        Console.WriteLine(slownik.getSize());

        Console.WriteLine("Removing second element... ");
        Console.WriteLine(slownik.remove("second"));
        slownik.print();
        Console.WriteLine("------");
        Console.WriteLine("Get third's value: ");


        Console.WriteLine(slownik.get("third"));
        // Console.WriteLine(slownik.get("for"));
        Console.WriteLine("\nIs empty: ");
        Console.WriteLine(slownik.isEmpty());

        Console.WriteLine("Clearing hash table... ");
        Console.WriteLine(slownik.remove("third"));
        Console.WriteLine(slownik.remove("first"));
        slownik.print();
        Console.WriteLine("\nIs empty: ");
        Console.WriteLine(slownik.isEmpty());
    }
}
