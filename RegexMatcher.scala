// Regular Expression Matcher
//==============================================

object RexpM {
  // some convenience for typing regular expressions
  import scala.language.implicitConversions    
  import scala.language.reflectiveCalls 

  // Definition of regular expressions
  abstract class Rexp
  case object ZERO extends Rexp
  case object ONE extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
  case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
  case class STAR(r: Rexp) extends Rexp         // star

  // The usual binary choice and binary sequence can be defined in terms of ALTs and SEQs
  def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
  def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))

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

  /*
    examples for the implicits:
    val test = ALT(CHAR('a'), CHAR('b'))
    val areg : Rexp = "a" | "b"

    SEQ(CHAR('a'), CHAR('b')) 
    val sreg : Rexp = "a" ~ "b"
  */

  // Check if a regular expression is nullable (can match an empty string)
  def nullable (r: Rexp) : Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(c) => false
    case ALTs(rs) => nullable(rs.head) || (if (rs.tail != Nil) nullable(ALTs(rs.tail)) else false) // there exists an r such that rs.nullable(r)
    case SEQs(rs) => nullable(rs.head) && (if (rs.tail != Nil) nullable(SEQs(rs.tail)) else true)  // for all r such that rs.nullable(r)
    case STAR(r) => true
  }

  // Compute the derivative of a regular expression with respect to a character
  def der (c: Char, r: Rexp) : Rexp = r match {
    case ONE => ZERO
    case ZERO => ZERO
    case CHAR(d) => if (c == d) ONE else ZERO 
    case ALTs(rs) => ALTs(der_helper(c, rs))
    case SEQs(rs) if rs.length == 0 => ZERO
    case SEQs(rs) => if (nullable(rs.head)) ALT(SEQs(List(der(c, rs.head)):::rs.tail), der(c, SEQs(rs.tail))) else SEQs(List(der(c, rs.head)):::rs.tail)
    case STAR(r) => SEQ(der(c, r), STAR(r))
  }

  def der_helper(c: Char, rs: List[Rexp], ans: List[Rexp] = Nil) : List[Rexp] = rs match {
    case Nil => ans
    case ONE::rest => der_helper(c, rest, ans:+ZERO)
    case ZERO::rest => der_helper(c, rest, ans:+ZERO)
    case CHAR(d)::rest => der_helper(c, rest, ans:+der(c, CHAR(d)))
    case ALTs(rs)::rest => der_helper(c, rest, ans:+der(c, ALTs(rs)))
    case SEQs(rs)::rest => der_helper(c, rest, ans:+der(c, SEQs(rs)))
    case STAR(r)::rest => der_helper(c, rest, ans:+der(c, STAR(r)))
  }

  // Remove nested alternatives and flatten the list of regular expressions
  def denest(rs: List[Rexp]) : List[Rexp] = rs match {
    case Nil => Nil
    case ZERO::rest => denest(rest)
    case ALTs(rs)::rest => rs:::denest(rest)
    case r::rest => r::denest(rest)
  }

  // Remove NIL and ZERO from the list of regular expressions
  def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = rs match {
    case Nil => acc
    case ZERO::rest => List(ZERO)
    case ONE::rest => flts(rest, acc)
    case SEQs(rs)::rest => flts(rest, acc:::rs)
    case r::rest => flts(rest, acc:::List(r))
  }

  def ALTs_smart(rs: List[Rexp]) : Rexp = rs match {
    case Nil => ZERO
    case r::Nil => r
    case _ => ALTs(rs)
  }

  def SEQs_smart(rs: List[Rexp]) : Rexp = rs match {
    case Nil => ONE
    case ZERO::Nil => ZERO
    case r::Nil => r
    case _ => SEQs(rs)
  }

  // Simplify a regular expression by denesting alternatives and flattening sequences
  def simp(r: Rexp) : Rexp = r match {
    case ALTs(rs) => ALTs_smart(denest(simp_helper(rs)).distinct)
    case SEQs(rs) => SEQs_smart(flts(simp_helper(rs)))
    case _ => r
  }

  def simp_helper(rs: List[Rexp], ans: List[Rexp]=Nil) : List[Rexp] = rs match {
    case Nil => ans
    case head::next => simp_helper(next, ans:+simp(head)) 
  }

  // Compute the derivative of a list of characters with respect to a regular expression
  def ders (s: List[Char], r: Rexp) : Rexp = s match {
    case Nil => r
    case c::cs => ders(cs, simp(der(c, r)))
  }

  // Match a string against a regular expression
  def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList, r))

  // Compute the size (number of nodes) of a regular expression
  def size(r: Rexp): Int = r match {
    case ZERO => 1
    case ONE => 1
    case CHAR(c) => 1
    case ALTs(rs) => 1 + size_helper(rs)
    case SEQs(rs) => 1 + size_helper(rs)
    case STAR(r) => 1 + size(r)
  }

  def size_helper(rs: List[Rexp], ans: Int = 0) : Int = rs match {
    case Nil => ans
    case ZERO::rest => size_helper(rest, ans+size(ZERO))
    case ONE::rest => size_helper(rest, ans+size(ONE))
    case CHAR(c)::rest => size_helper(rest, ans+size(CHAR(c)))
    case ALTs(rs)::rest => size_helper(rest, ans+size(ALTs(rs)))
    case SEQs(rs)::rest => size_helper(rest, ans+size(SEQs(rs)))
    case STAR(r)::rest => size_helper(rest, ans+size(STAR(r)))
  }

}