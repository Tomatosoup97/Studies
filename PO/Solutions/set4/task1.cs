using System;


interface IEnumerable<T> {
    protected int Length = 0;

    public T get(int index);

    public T remove(T obj);

    public void create(T obj);

    public string ToString();

    public T this[int index] {
        get {
            return this.get(index);
        }
        set {
            T obj = this.get(index);
            obj.val = value;
        }
    }

    public bool isEmpty() {
        return this.Length == 0;
    }

    public int getLength() {
        return this.Length;
    }
}

public class Elem<T> {
    public T val;
    public Elem<T> next;
    public Elem<T> prev;

    public ToString() {
        return this.val.ToString();
    }

    public Elem(T val) {
        // [...]
    }
}


public class Lista<T> : IEnumerable {
    protected Elem<T> first;
    protected Elem<T> last;

    public void ToString() {
        string result = "";
        result += "[ ";
        do {
            result += (last.ToString() + ", ");
        } while (this.last.next != null);
        result += " ]";
        return result
    }

    public void pushFront(T val) {
        // [...]
    }

    public void pushBack(T val) {
        // [...]
    }

    public void create(T val) {
        // push()
    }

    public T popFirst() {
        // [...]
    }

    public T popLast() {
        // [...]
    }

    public T remove() {
        // pop()
    }
}

using System;


public class HashNode<K, V> {
    public K key;
    public V val;
    public HashNode<K, V> next;

    public HashNode(K key, V val) {
        // [...]
    }

    public void setOrUpdate(K key, V val) {
        // [...]
    }
}


public class Slownik<K, V> implement {
    protected HashNode<K, V>[] bucket;
    protected int capacity;
    protected int size;

    public void allocateNewBucket(int capacity) {
        // [...]
    }

    public Slownik() {
        this.capacity = 8;
        this.allocateNewBucket(this.capacity);
    }

    public void ToString() {
        string result = "";
        result += "{ ";
        for (int i=0; i<this.capacity; i++) {
            if (this.bucket[i] != null) {
                result += (this.bucket[i].key + ": " +
                           this.bucket[i].val + ", ");
            }
        }
        result += " }";
        return result;
    }

    protected int getBucketIndex(K key) {
        // [...]
    }

    protected double getLoadFactor() {
        // [...]
    }

    protected void adjustCapacity() {
        // [...]
    }

    public void create(HashNode<K, V> node) {
        // [...]
    }

    public V get(K key) {
        // [...]
    }

    public V remove(K key) {
        // [...]
    }
}
