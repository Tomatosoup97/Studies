using System;


public class IntStream {
   public int num = 0;

   public int next() {
      this.num += 1;
      return this.num;
   }

   public bool eos() {
      if (this.num == 2147483647) {
         return true;
      }
      return false;
   }

   public void reset() {
      this.num = 0;
   }
}


public class PrimeStream : IntStream {
   bool is_prime(int n) {
      for (int i=2; i<Math.Sqrt(n)+1; i++) {
         if (n % i == 0) {
            return false;
         }
      }
      return true;
   }

   public new int next() {
      int prime = 1;
      while(!this.eos()) {
         if (this.is_prime(this.num)) {
            prime = this.num;
            this.num += 1;
            break;
         }
         this.num += 1;
      }
      return prime;
   }
}


public class RandomStream : IntStream {
   public Random rand;

   public RandomStream() {
      this.rand = new Random();
   }

   public new int next() {
      return this.rand.Next(65, 122);
   }

   public new bool eos() {
      return false;
   }
}


public class RandomWordStream {
   public RandomStream randStream;
   public PrimeStream primeStream;

   public RandomWordStream(RandomStream randStream, PrimeStream primeStream) {
      this.randStream = randStream;
      this.primeStream = primeStream;
   }

   public string next() {
      int len = this.primeStream.next();
      string result = "";
      for (int i=0; i < len; i++) {
         result += (char) this.randStream.next();
      }
      return result;
   }
}


public class Program {
   public static void Main(string[] args) {
      IntStream intStream;
      intStream = new IntStream();

      Console.WriteLine("\nTesting IntStream: \n");

      for (int i=0; i<10; i++) {
         Console.WriteLine(intStream.next());
      }

      Console.WriteLine("\nTesting PrimeStream: \n");
      PrimeStream primeStream;
      primeStream = new PrimeStream();

      for (int i=0; i<10; i++) {
         Console.WriteLine(primeStream.next());
      }

      Console.WriteLine("\nTesting RandomStream: \n");
      RandomStream randomStream;
      randomStream = new RandomStream();

      for (int i=0; i<10; i++) {
         Console.WriteLine(randomStream.next());
      }

      primeStream.reset();
      Console.WriteLine("\nTesting RandomWordStream: \n");
      RandomWordStream randomWordStream;
      randomWordStream = new RandomWordStream(randomStream, primeStream);

      for (int i=0; i<10; i++) {
         Console.WriteLine(randomWordStream.next());
      }
   }
}
