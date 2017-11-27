// Part 1 about Regular Expression Matching
//==========================================


object CW8a {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}




  def nullable(r: Rexp): Boolean = r match{

      case ZERO => false

      case ONE => true

      case CHAR(r) => false

      case ALT(r1, r2) => nullable(r1) || nullable(r2)

      case SEQ(r1, r2) => nullable(r1) && nullable(r2)

      case STAR(r) => true



    }


  def der (c: Char, r: Rexp) : Rexp = r match{

    case ZERO => ZERO

    case ONE => ZERO

    case CHAR(r) => {
      if (c == r) ONE
      else ZERO
    }

    case ALT(r1, r2) => ALT(der(c, r1) , der(c, r2))

    case SEQ(r1, r2) =>{

      if(nullable(r1)) ALT(SEQ(der(c, r1) , r2 ) , der(c, r2))
      else SEQ(der(c, r1),  r2)

    }

    case STAR(r) => SEQ(der(c, r), STAR(r))


  }




  def simp(r: Rexp) : Rexp = r match {


    case CHAR(r) => CHAR(r)

    case ZERO => ZERO

    case ONE => ONE

    case SEQ(r, ZERO) => ZERO

    case SEQ(ZERO, r) => ZERO

    case SEQ(r , ONE) => simp(r)

    case SEQ(ONE, r) => simp(r)

    case ALT(r , ZERO) => simp(r)

    case ALT(ZERO, r) => simp(r)


    case ALT(r1, r2) => {

      if (r1 == r2) simp(r1)
      else if (r1 == ONE) ALT(r1, r2)
      else if (r2 == ONE) ALT(r1, r2)
      else simp(ALT(simp(r1), simp(r2)))

    }


    case STAR(r) => STAR(r)



  }




  def ders (s: List[Char], r: Rexp) : Rexp = s match {

    case Nil => r

    case c :: i => {


      ders(i, simp(der(c, r)))


    }



  }

  def matcher(r: Rexp, s: String): Boolean = {

    nullable(ders(s.toList, r))


  }


// (1e) Complete the size function for regular
// expressions according to the specification 
// given in the coursework.

//def size(r: Rexp): Int = ...


// some testing data

/*
matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false

// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000 ++ "b")   // => true
matcher(EVIL, "a" * 1000)          // => false

// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Java needs around 30 seconds for matching 28 a's with EVIL. 
//
// Lets see how long it takes to match strings with 
// 0.5 Million a's...it should be in the range of some
// seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 0 to 5000000 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}

*/


}
