using System;


public class HashNode<K, V> {
    public K key;
    public V val;
    public HashNode<K, V> next;

    public HashNode(K key, V val) {
        this.key = key;
        this.val = val;
    }
}


public class Slownik<K, V> {
    /* Hash Map.
       Uses separate chaining for handling collisions.
    */
    protected HashNode<K, V>[] bucket;
    protected int capacity;
    protected int size;

    public void allocateNewBucket(int capacity) {
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

    public bool isEmpty() {
        return this.size == 0;
    }

    public int getSize() {
        return this.size;
    }

    public int getBucketIndex(K key) {
        int hashCode = key.GetHashCode();
        return hashCode % capacity;
    }

    protected double getLoadFactor() {
        return this.size * 1.0 / this.capacity;
    }

    protected void adjustCapacity() {
        // Increase size of the bucket if load factor is too big
        double loadFactor = this.getLoadFactor();
        if (loadFactor >= 0.7) {
            this.capacity *= 2;
            allocateNewBucket(this.capacity);
        }
    }

    public void create(K key, V val) {
        int index = getBucketIndex(key);
        HashNode<K, V> head = bucket[index];

        // If key is present, update value
        while (head.next != null) {
            if (head.key.Equals(key)) {
                head.val = val;
                return;
            }
            head = head.next;
        }
        HashNode<K, V> newNode = new HashNode<K, V>(key, val);
        head = newNode;
        size++;
        adjustCapacity();
    }

    public V get(K key) {
        int index = getBucketIndex(key);
        HashNode<K, V> head = bucket[index];
        
        // Search key in chain
        while (head != null) {
            if (head.key.Equals(key))
                return head.val;
            head = head.next;
        }
        throw new System.Exception("Key Error: Element not found");
    }

    public V remove(K key) {
        int index = getBucketIndex(key);
        HashNode<K, V> head = bucket[index];
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
        else bucket[index] = head.next;
        return head.val;
    }
}


public class Program {
    public static void Main(string[] args) {
        Console.WriteLine("Hash Map: \n");
        Slownik<string, int> slownik = new Slownik<string, int>();
        slownik.create("some", 1);
        slownik.create("keys", 2);
        slownik.create("for", 4);
        slownik.create("hashmap", 5);
        Console.WriteLine(slownik.getSize());
        Console.WriteLine(slownik.remove("some"));
        Console.WriteLine(slownik.remove("keys"));
        Console.WriteLine(slownik.get("for"));
        Console.WriteLine(slownik.getSize());
        Console.WriteLine(slownik.isEmpty());
    }
}
