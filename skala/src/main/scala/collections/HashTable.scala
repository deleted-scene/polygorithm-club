package collections

/*
 //TODO: ref: OpenJDK
 static int hash(int h) {
     // This function ensures that hashCodes that differ only by
     // constant multiples at each bit position have a bounded
     // number of collisions (approximately 8 at default load factor).
     h ^= (h >>> 20) ^ (h >>> 12);
     return h ^ (h >>> 7) ^ (h >>> 4);
 }

 */
class HEntry[T, K](val key: T, val value: K)

class HashTable[T, K]() {
  /*prime number*/
  private var size: Int = 37

  private var buckets: Array[List[HEntry[T, K]]] = Array.fill[List[HEntry[T, K]]](size) {
    List[HEntry[T, K]]()
  }

  private def indexOf(key: T): Int = key.hashCode() % buckets.length

  def put(key: T, value: K): Unit = {
    val idx = indexOf(key)
    val bucket = buckets(idx)

    for (i <- bucket.indices) {
      val entry = bucket(i)
      if (entry.key.equals(key)) {
        buckets(idx) = bucket.patch(i, Seq(new HEntry(key, value)), 1)
      }
    }

    //todo:  resize

    buckets(idx) = new HEntry(key, value) :: bucket
  }

  def get(key: T): K = {
    val idx = indexOf(key)
    val bucket = buckets(idx)

    for (i <- bucket.indices) {
      val entry = bucket(i)
      if (entry.key.equals(key)) return entry.value
    }

    throw new Exception("Key not found")
  }


  def remove(key: T): Unit = {
    val idx = indexOf(key)
    val bucket = buckets(idx)

    for (i <- bucket.indices) {
      val entry = bucket(i)
      if (entry.key.equals(key)) {
        buckets(idx) = bucket.patch(i, null, 1)
      }
    }

    //todo:  resize
    throw new Exception("Key not found")
  }

}
