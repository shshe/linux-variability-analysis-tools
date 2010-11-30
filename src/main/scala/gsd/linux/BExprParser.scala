package gsd.linux

/**
 * Created by IntelliJ IDEA.
 * User: shshe
 * Date: 29-Nov-2010
 * Time: 10:30:22 PM
 * To change this template use File | Settings | File Templates.
 */
import util.parsing.combinator._
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

object BExprParser extends RegexParsers with PackratParsers with ImplicitConversions {

  override val whiteSpace = """[\t\f ]+""".r

  case class BExprResult(ids: List[String], 
                         generated: List[String],
                         BExprs: List[BExpr])

  val nl = """[\r]?\n""".r
  val ids  = rep("@" ~> """\w+""".r <~ rep1(nl))
  val gens = rep("$" ~> """\w+""".r <~ rep1(nl))

  private lazy val exprresults = ids ~ gens ~
    (rep(orExpr <~ rep1(nl)) ^^  { _ filterNot { _ == BTrue } }) ^^ BExprResult


  private lazy val orExpr : PackratParser[BExpr] =
    andExpr ~ rep("|".r ~> orExpr) ^^
      {
        case x~ys => (x /: ys){ BOr }
      }
  private lazy val andExpr : PackratParser[BExpr] =
    implExpr ~ rep("&".r ~> andExpr) ^^
      {
        case x~ys => (x /: ys){ BAnd }
      }
  private lazy val implExpr : PackratParser[BExpr] =
    biimpExpr ~ rep(("->"|"=>") ~> implExpr) ^^
      {
        case x~ys => (x /: ys){ BImplies }
      }
  private lazy val biimpExpr : PackratParser[BExpr] =
    unaryExpr ~ rep(("<->"|"<=>") ~> biimpExpr) ^^
      {
        case x~ys => (x /: ys){ (x,y) => BAnd(BImplies(x,y), BImplies(y,x)) }
      }
  private lazy val unaryExpr : PackratParser[BExpr] =
    ("!" ~> unaryExpr) ^^ BNot | primary
  
  private lazy val primary : PackratParser[BExpr] =
    """\w+""".r ^^
      {
        case "1" => BTrue
        case "0" => BFalse
        case n => BId(n)
      } |
    "(" ~> orExpr <~ ")"

  def parseBExpr(str: String) = succ(parseAll(orExpr, str))

  def parseBExprFile(file: String) : BExprResult =
    succ(parseAll(exprresults, new PagedSeqReader(PagedSeq fromFile file)))

  protected def succ[A](p : ParseResult[A]) = p match {
    case Success(res,_) => res
    case x => error(x.toString)
  }

}
