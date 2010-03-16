
object SpamPlan {
  import probability._
  import probability.EmbeddedProbability._

  sealed abstract class Classification
  case class Spam() extends Classification { override def toString = "Spam" }
  case class Ham()  extends Classification { override def toString = "Ham" }

  val S = List(Spam(), Ham())

  private val uniformClasses : Distribution[Classification] = prob { uniform(S) }

  trait SpamFeaturesDB {
    // these form a beta distribution
    def corpusSize : Int = spamCount + hamCount
    def spamCount  : Int
    def hamCount   : Int
    def wordCount(word:String, t:Classification) : Int

    def knownWords : Iterator[String]

    def findClassifiers(words:Iterator[String], max:Int) : Iterator[Distribution[Classification]]

    def countType(t:Classification) : Int = t match {
      case _:Spam => spamCount
      case _:Ham => hamCount
    }

    // P(S)
    def pMsgType = dist((spamCount, Spam()), (hamCount, Ham()))

    // P(W == word | S == type)
    def pWord(word:String, t:Classification) = {
      val n:Double     = wordCount(word, t)
      val total:Double = countType(t)
      flip(n/total)
    }

    def pHasWord(word:String, t:Classification) = {
      val tmp = pWord(word, t)
      if(tmp) Some(t) else None
    }

    // P(S | W == word) = < P(W == word | S) * prior) >
    def pHasWord(word:String, prior:Distribution[Classification] = prob(pMsgType)) = {
      var t = dist(prior)
      /*if (pWord(word, t)) Some(t) else None*/
      var tmp = pWord(word, t)
      if (tmp) Some(t) else None
    }

    // P(S | W1 == word1, W2 == word2, ... ) 
    //      = < P(S|W1) * P(S|W2) * ... >
    def pHasWords(words:Iterator[String], prior:Distribution[Classification] = prob(pMsgType)) = {
      val clazz:Option[Classification] = dist(prior.map { Some(_) })
      words.foldLeft(clazz) { (optT, word) =>
        optT flatMap (pHasWord(word, _))
      }
    }
      //words.foldLeft(prior) { (p,word) => prob { pHasWord(word, p) }}

    def characteristic(f: Distribution[Classification] => Distribution[Classification]) = {
      val tmp = f(uniformClasses)
      tmp
    }

    def score(f:Distribution[Classification] => Distribution[Classification]) = {
      uniformClasses.distance( characteristic(f) )
    }

  }


  object SpamTestDB extends SpamFeaturesDB {
    def hamCount = 102
    def spamCount = 57

    val words : Map[String,(Int,Int)] = Map(
        "the"    -> (10, 20),
        "quick"  -> (11,11),
        "brown"  -> (0,15),
        "fox"    -> (0, 10),
        "jumps"  -> (0, 10),
        "over"   -> (0, 10),
        "lazy"   -> (0, 10),
        "dog"    -> (0, 10),
        "make"   -> (10, 0),
        "money"  -> (20, 0),
        "in"     -> (15, 25),
        "online" -> (20, 5),
        "casino" -> (40, 1),
        "free"   -> (52, 4),
        "bayes"  -> (1, 10),
        "monad"  -> (0, 22),
        "hello"  -> (30, 32),
        "asdf"   -> (40, 2)
    )

    case class Classifier(score:Double, d:Distribution[Classification]);

    //val classifiers : Map[String,Classifier] = new scala.collection.immutable.HashMap[String,Classifier]
    val classifiers : Map[String,Classifier] = words.map { tmp =>
      val word = tmp._1
      val s = score { prior => normalizedProb[Classification] { 
                        pHasWord(word, prior) 
                    }}
      val d = normalizedProb[Classification] { 
          pHasWord(word, uniformClasses) 
      }
      word -> Classifier(s, d.adjustProbabilisticMinimum(0.01))
    }

    def knownWords = words.keysIterator

    def wordCount(word:String, t:Classification) = {
      val tmp = words.getOrElse(word, (0,0))
      t match {
        case _:Spam => tmp._1
        case _:Ham  =>  tmp._2
      }
    }

    def findClassifiers(words:Iterator[String], max:Int) : Iterator[Distribution[Classification]] = 
      words.map{ word => classifiers.get(word) }.
            filter{ _.isDefined }.
            map{ _.get }.
            toList.
            sortBy { _.score }(Ordering[Double].reverse).
            map{ _.d }.
            iterator.take(max)
  }

  /*
   * P(S|Words) = < P(S|W1) * P(S|W2) * ... >
   */
  def naiveSpamClassifier_(db:SpamFeaturesDB, 
                          words:Iterator[String], 
                          max:Int = 15,
                          prior:Distribution[Classification] = null) = {
    val prior_ : Distribution[Option[Classification]] = 
                    (if (prior == null) {
                       prob{db.pMsgType}} 
                     else prior).map(Some(_))
    val classifiers = db.findClassifiers(words,max).toList
    val p = classifiers.foldLeft(prior_) { (cur, d) =>
      cur.dep { optT:Option[Classification] =>
        prob[Option[Classification]] {
          optT match {
            case None => None
            case Some(t) => {
              val tmp = flip(d.probability(t))
              if (tmp) Some(t) else None
            }
          }}}}
    (Probability.normalize(p), classifiers.length)
  }

  def naiveSpamClassifier(db:SpamFeaturesDB, 
                          words:Iterator[String], 
                          max:Int = 15,
                          prior:Distribution[Classification] = null) = {
    naiveSpamClassifier_(db, words, max, prior)._1
  }

  def fisherClassifier(db:SpamFeaturesDB,
                        words:Iterator[String],
                        max:Int = 15,
                        prior:Distribution[Classification] = null) = {

    val (hypothesis,dof_2) = naiveSpamClassifier_(db, words, max, prior)

    val h = hypothesis.iterator.map { tmp =>
        val clazz = tmp._1
        val p     = tmp._2

        import scala.math._
        val m = -log(p)
        var t = p
        val s = (1 until dof_2).foldLeft(t) { (sum, i) =>
          t *= m / (i.asInstanceOf[Double])
          sum + t
        }
        (clazz -> (if (s < 1.0) s else 1.0))
    }

    val m = new scala.collection.mutable.HashMap[Classification, Double]
    h.foreach { tmp =>
      val clazz = tmp._1
      val p     = tmp._2

      S.foreach{ other =>
        if (clazz != other) {
          m += other -> (1.0 - p + m.getOrElse(other, 0.0)) 
        }
      }
    }

    Distribution[Classification](m)
  }

  def run = {
    val testCorpus = List(
        List("free"),
        List("monad"),
        List("free", "monad"),
        List("free", "monad", "dog"),
        List("free", "asdf", "online", "bayes", "quick", "jumps", "test", "fox"),
        List("free", "monad", "asdf", "bayes", "quick"))

    testCorpus.foreach { data => 
      println("data: " + data);

      println("naive bayesian spam classifier:")
      println(naiveSpamClassifier(SpamTestDB, data.iterator))

      println("fisher method:")
      println( fisherClassifier(SpamTestDB, data.iterator) )

      println("\n")
    }
  }

  def main(args:Array[String]) = run

}

