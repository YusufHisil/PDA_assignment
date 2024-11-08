import scala.io.Source

@main
def main(): Unit = {
  val source = Source.fromFile("resources/pg11.txt")
  val content = source.mkString
  source.close()

  val v_freq = freq(content)
  val wordCount = v_freq.foldLeft(0) { (acc, curr) => acc + curr._2 }

  println("FREQUENCY")
  println(s"wordCount: $wordCount \t different ones: ${v_freq.length}")
  println("Word\t\toccurences\t\tfrequency")
  println("-" * 40)
  v_freq.take(10).foreach(x => println(f"${x._1}%-15s" + f"${x._2}%-15d" + f"${100 * x._2 / wordCount.toFloat}%.2f"))


  val v_nonstopfreq = nonstopfreq(content)
  println("\n"*3 + "FREQUENCY WITHOUT STOPWORDS")
  println(s"wordCount: $wordCount \t different ones: ${v_nonstopfreq.length}")
  println("Word\t\toccurences\t\tfrequency")
  println("-" * 40)
  v_nonstopfreq.take(10).foreach(x => println(f"${x._1}%-15s" + f"${x._2}%-15d" + f"${100 * x._2 / wordCount.toFloat}%.2f"))

  val v_freqfreq = wordfreqfreq(content)
  println("\n"*3 + "FREQFREQ")
  println("the 10 most frequent frequencies")
  v_freqfreq.take(10).foreach(x => println(s"${x._2} words appear ${x._1} times"))
  println("\nthe 5 least frequent frequencies")
  v_freqfreq.takeRight(5).foreach(x => println(s"${x._2} words appear ${x._1} times"))

  val v_ngrams = ngrams(content, 3)
  println("\n" * 3 + "NGRAMS")
  v_ngrams.take(10).foreach(x => println(f"${x._1}%-40s${x._2}%d"))

  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/pg12.txt", 1))
  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/pg12.txt", 2))
  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/pg12.txt", 3))

  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/alice_in_wonderland.txt", 1))
  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/alice_in_wonderland.txt", 2))
  println("\n" * 3 + cos_sim("resources/pg11.txt", "resources/alice_in_wonderland.txt", 3))

  println("\n" * 3 + cos_sim("resources/alice_in_wonderland.txt", "resources/pg12.txt", 1))
  println("\n" * 3 + cos_sim("resources/alice_in_wonderland.txt", "resources/pg12.txt", 2))
  println("\n" * 3 + cos_sim("resources/alice_in_wonderland.txt", "resources/pg12.txt", 3))
}

def cos_sim(book1: String, book2: String, n: Int): Double =
{
  //opens the documents and transforms them into Strings for easy processing
  val s1 = Source.fromFile(book1)
  val s2 = Source.fromFile(book2)
  val c1 = s1.mkString
  val c2 = s2.mkString
  s1.close()
  s2.close()

  //gathers the ngrams from each document with the ngrams method
  val words1: List[(String, Int)] = ngrams(c1, n)
  val words2: List[(String, Int)] = ngrams(c2, n)

  //maximum frequencies, used to calculate normalized frequencies later
  val max_freq1: Int = words1.maxBy(_._2)._2
  val max_freq2: Int = words2.maxBy(_._2)._2

  // obtain a map of all the n-grams in both the documents, but only with the frequencies
  // of the first document, so that they can be compared for all the words
  val map1: Map[String, Int] = words1.toMap
  val complete_map1 :Map[String, Int] = words2.map {
    case (word, freq) => (word, map1.getOrElse(word, 0))
  }.toMap ++ map1

  val map2 = words2.toMap
  val complete_map2: Map[String, Int] = words1.map {
    case (word, freq) => (word, map2.getOrElse(word, 0))
  }.toMap ++ map2

  // turn the maps into vectors for cosinus similarity analysis
  // the normalized frequency is calculated
  // the vectors are sorted by the words so that both vectors have the same words on the same indices
  val a: Vector[(String, Float)] = complete_map1
    .toVector
    .map{
    case (word, freq) => (word, freq.toFloat/max_freq1)
  }
    .sortBy(_._1)

  val b: Vector[(String, Float)] = complete_map2
    .toVector
    .map {
      case (word, freq) => (word, freq.toFloat / max_freq2)
    }
    .sortBy(_._1)

  // calculate the components of the cosinus similarity formula separately, for enhanced readability
  val dotProduct: Double = (a zip b).map { case (x, y) => x._2 * y._2 }.sum
  val length1: Double = math.sqrt(a.map(x => x._2*x._2).sum)
  val length2: Double = math.sqrt(b.map(x => x._2*x._2).sum)

  // calculate and return the similarity index
  dotProduct/(length1 * length2)
}

def ngrams(input: String, n: Int): List[(String, Int)] = {
  val s_stop = Source.fromFile("resources/english-stop.txt")
  val stop_words = s_stop.mkString.split("\\s+") // returns a list of all the stop words
  s_stop.close()

  input
    .toLowerCase()
    .replaceAll("[^a-zA-Z\\s]", " ")
    .split("\\s+")
    .filterNot(stop_words.contains(_))// filters out the stop-words
    .sliding(n)// returns an iterator with a sliding window of n elements that goes over the Array[String]
    .toList
    .map(_.mkString(" "))// merges each array resulted from the iterator into a string, using " " as a joint
    .groupBy(identity)// makes a map where (k,v) is (n-gram, occurrences)
    .map{case (ngram, occurrences) => (ngram, occurrences.size)}// counts the frequency of occurrence
    .toList
    .sortWith((e1,e2) => e1._2 > e2._2)// sorts for easier displaying
}

def wordfreqfreq(input: String): List[(Int, Int)] = {
  input.toLowerCase()
    .replaceAll("[^a-zA-Z\\s]", " ")
    .split("\\s+")
    .groupBy(x => x)// => map[word, Array[Word]] where the second array holds every occurrence of the key
    .map(tuple => (tuple._1, tuple._2.length))// map[word, frequency] by counting the number of occurrences
    .groupBy(kv => kv._2)// map[freq, map[word, freq]]; turned into a map based on frequency, the inner map holds a list of occurrences of each frequency
    .map{case (freq, wordlist) => (freq, wordlist.size)}// map[Int, Int] where the second is the freqfreq, as counted by the length of the occurrences of each freq
    .toList
    .sortWith((e1, e2) => e1._2 > e2._2)
}

def nonstopfreq(input: String): List[(String, Int)] = {
  val source = Source.fromFile("resources/english-stop.txt")
  val content = source.mkString.split("\\s+") // returns a list of all the stop words
  source.close()

  freq(input).filterNot(e1 => content.contains(e1._1))
}

def freq(input: String): List[(String, Int)] = {
  input
    .toLowerCase()
    .replaceAll("[^a-zA-Z\\s]", " ")
    .split("\\s+")
    .groupBy(x=>x)
    .map(tuple => (tuple._1, tuple._2.length))
    .toList
    .sortWith((e1, e2) => e1._2 > e2._2)
}