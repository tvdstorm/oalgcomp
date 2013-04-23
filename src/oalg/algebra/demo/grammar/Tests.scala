package oalg.algebra.demo.grammar

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import oalg.algebra.demo.grammar.Grammar.grammarFirst
import oalg.algebra.demo.grammar.Grammar.grammarPrint
import oalg.algebra.demo.grammar.Grammar.grammarParse
import oalg.algebra.demo.grammar.Grammar.grammarNullable
import oalg.algebra.demo.grammar.Grammar.grammarProfile
import oalg.algebra.demo.grammar.Grammar.Nullable
import oalg.algebra.demo.grammar.Grammar.Print
import oalg.algebra.demo.grammar.Grammar.Parse
import oalg.algebra.demo.grammar.Grammar.Profile
import oalg.algebra.demo.grammar.Grammar.First
import oalg.algebra.demo.grammar.Grammar.Memo
import oalg.algebra.demo.grammar.Grammar.Trace
import oalg.algebra.demo.grammar.Grammar.CircFirst
import oalg.algebra.demo.grammar.Grammar.CircNullable
import oalg.algebra.demo.grammar.Grammar.GrammarAlg
import oalg.algebra.demo.grammar.Grammar.OpenGrammarAlg
import oalg.algebra.demo.grammar.Grammar.GrammarComb._
import oalg.algebra.core.Algebras.Lifter
import java.io.StringWriter
import oalg.algebra.demo.grammar.Grammar.Print
import java.io.PrintWriter
import java.io.Writer
 
class GrammarSuite extends FunSuite {
  val exp = Seq("x", "+", "x", "+", "x", "*", "x")
  val as = Seq("a", "a", "a")
  
  def makeIterA[X](f: GrammarAlg[X, X]): Map[String,X] = {
    // A ::= | "a" A
    val e = f.Empty
    val a = f.NonTerminal("A")
    val w = f.Terminal("a")
    Map("A" -> f.Alt(e, f.Seq(w, a)))
  }
  
  def makeLeftRec[X](f: GrammarAlg[X, X]): Map[String, X] = {
    // Exp ::= Exp "+" Exp | Exp "*" Exp | "?"? Exp | "x"
    val exp = f.NonTerminal("Exp")
    val plus = f.Terminal("+")
    val star = f.Terminal("*")
    val add = f.Seq(exp, f.Seq(plus, exp))
    val mul = f.Seq(exp, f.Seq(star, exp))
    val opt = f.Seq(f.Opt(f.Terminal("?")), exp)
    val x = f.Terminal("x")
    Map("Exp" -> f.Alt(add, f.Alt(mul, f.Alt(opt, x))))
  }
  
  
  def parse(g: Map[String,Parse], p: Parse, input: Seq[String]): Boolean = {
    try {
      p.parse(g, input, rest => {
        if (rest.isEmpty) {
          //System.out.println("Success")
          throw new RuntimeException("Success")
        }
        else {
          //System.out.println("Success, remaining: " + rest)
        }
      })
    }
    catch {
      case e:RuntimeException => return true
    }
    return false;
  }
  
  test("Memo decoration allows left recursion") {
    val alg = fclose(decorate(grammarParse[Parse], new Memo))
    val grammar = makeLeftRec[Parse](alg)
    assert(parse(grammar, grammar("Exp"), exp) == true)
  }
    
  test("Stack overflow on left recursion without memo") {
    val exp = Seq("x", "+", "x", "+", "x", "*", "x")
    val alg = fclose(grammarParse[Parse])
    val grammar = makeLeftRec[Parse](alg)
    val thrown = intercept[StackOverflowError] {
      parse(grammar, grammar("Exp"), exp)
    }
    assert(thrown.isInstanceOf[StackOverflowError])
  }
  
  
  test("Right recursion using optional alternative") {
    val alg = fclose(grammarParse[Parse])
    val grammar = makeIterA[Parse](alg);
    assert(parse(grammar, grammar("A"), as) == true);
  }
  
  test("Parsing continues to end of string") {
    val as = Seq("a", "a", "b")
    val alg = fclose(grammarParse[Parse])
    val grammar = makeIterA[Parse](alg);
    assert(parse(grammar, grammar("A"), as) == false);
  }
  
  test("Trace decoration outputs trace") {
    val as = Seq("a")
    val writer = new StringWriter
    val alg = fclose(decorate(grammarParse[Parse], new Trace(writer)))
    val grammar = makeIterA[Parse](alg)
    assert(parse(grammar, grammar("A"), as) == true)
    assert(writer.toString() == "[[][[[[[")
  }
  
  
  test("Profile is constructed after merging profile with parse") {
    val step1 = merge[Parse, Profile, PPP](
                  LiftParseProfile, // variance issue if not use PPP 
                  decorate(grammarParse[PPP], new Memo),
                  grammarProfile[PPP])
    
    // fclose expects S, S not, e.g.,  A with B and AwithB
    val oap = fclose(step1.asInstanceOf[OpenGrammarAlg[PPP, PPP]]) 
    val gprof = makeLeftRec(oap)
    assert(parse(gprof, gprof("Exp"), exp) == true)
    assert(gprof("Exp").profile(gprof("Exp")) > 0)
  }
  
  test("Just printing a simple grammar") {
    val alg = fclose(grammarPrint[Print])
    val grammar = makeIterA(alg)
    assert(grammar("A").toString() == "()|(\"a\" A)")
  }

  test("Printing a left-recursive grammar with non-desugared optionals") {
    val alg = fclose(grammarPrint[Print])
    val grammar = makeLeftRec(alg)
    assert(grammar("Exp").toString() == "(Exp \"+\" Exp)|((Exp \"*\" Exp)|(((\"?\")? Exp)|(\"x\")))");
  }

  
  test("Parsing with profiling with printing") {
    val step1 = merge[Parse, Profile, PPP](
                  LiftParseProfile, // variance issue if not use PPP 
                  decorate(grammarParse[PPP], new Memo),
                  grammarProfile[PPP])

    val step2 = merge[Parse with Profile, Print, PPP](
    			  LiftParseProfilePrint, 
    			  step1, 
    			  grammarPrint[PPP])
    
    val oap = fclose(step2.asInstanceOf[OpenGrammarAlg[PPP, PPP]]) 
    val gprof = makeLeftRec(oap)
    assert(parse(gprof, gprof("Exp"), exp) == true)
    assert(gprof("Exp").profile(gprof("Exp")) > 0)
    assert(gprof("Exp").toString() == "(Exp \"+\" Exp)|((Exp \"*\" Exp)|(((\"?\")? Exp)|(\"x\")))")
  }

  test("Nullable returns true on nullable non-terminals") {
    val grammar = makeIterA[Nullable](fclose(grammarNullable[Nullable]))
    assert(grammar("A").nullable(grammar) == true);
  }

  test("Left-recursive rule is not nullable") {
    val grammar = makeLeftRec[Nullable](fclose(decorate(grammarNullable[Nullable], new CircNullable)))
    assert(grammar("Exp").nullable(grammar) == false)
  }

  test("First-set of right recursive grammar") {
    val grammar = makeIterA(fclose(
        combine[Nullable, First, NF](
            decorate[Nullable, NF](grammarNullable[NF], new CircNullable),
            decorate[First, NF](grammarFirst[NF], new CircFirst)).asInstanceOf[OpenGrammarAlg[NF,NF]]))
    assert(grammar("A").first(grammar) == Set("a"))
  }

  test("First-set of left-recursive grammar") {
    val grammar = makeLeftRec(fclose(
        combine[Nullable, First, NF](
            decorate(grammarNullable[NF], new CircNullable),
            decorate(grammarFirst[NF], new CircFirst)).asInstanceOf[OpenGrammarAlg[NF,NF]]))

    assert(grammar("Exp").first(grammar) == Set("?", "x"))
  }
  
  test("Parsing with nullable") {
    val oa = fclose(
      combine[Parse, Nullable, PN](
        decorate[Parse, PN](grammarParse[PN], new Memo),
        decorate[Nullable, PN](grammarNullable[PN], new CircNullable)).asInstanceOf[OpenGrammarAlg[PN,PN]])
    
    val g1 = makeIterA[PN](oa);
    assert(parse(g1, g1("A"), as) == true)
    assert(g1("A").nullable(g1) == true)
    
    val g2 = makeLeftRec[PN](oa);
    assert(parse(g2, g2("Exp"), exp) == true)
    assert(g2("Exp").nullable(g2) == false)
  }
  
  test("Parsing with nullable with first") {
    val oa = fclose(
        combine[Parse, Nullable with First, PNF](
        decorate(grammarParse[PNF], new Memo),
         combine[Nullable, First, PNF](
            decorate(grammarNullable[PNF], new CircNullable),
            decorate(grammarFirst[PNF], new CircFirst))).asInstanceOf[OpenGrammarAlg[PNF,PNF]])
            
    val g1 = makeIterA[PNF](oa);
    assert(parse(g1, g1("A"), as) == true)
    assert(g1("A").nullable(g1) == true)
    assert(g1("A").first(g1) == Set("a"))
    
    val g2 = makeLeftRec[PNF](oa);
    assert(parse(g2, g2("Exp"), exp) == true)
    assert(g2("Exp").nullable(g2) == false)
    assert(g2("Exp").first(g2) == Set("?", "x"))
  }
  
  def powerset[A](s: Set[A]) = s.foldLeft(Set(Set.empty[A])) { case (ss, el) => ss ++ ss.map(_ + el) }
  
  def isValid(fs: Set[String]): Boolean = {
    if (fs.isEmpty) {
      return false;
    }
    if (fs.contains("First") && !fs.contains("Nullable")) {
      return false
    }
    if (fs.contains("Trace") && !fs.contains("Parse")) {
      return false
    }
    if (fs.contains("Profile") && !fs.contains("Parse")) {
      return false
    }
    val fs2 = fs - "Trace"
    return fs2.size <= 2
  }
  
  def runGrammar(fs: Set[String], grammar: Map[String,All], 
		  	fixt: AllConfFixture) {
    if (fs.contains("Parse")) {
      assert(parse(grammar, grammar(fixt.start), fixt.src) == true)
      if (fs.contains("Trace")) {
        assert(fixt.trace.toString() != "");
      }      
      if (fs.contains("Profile")) {
        assert(grammar(fixt.start).profile(grammar(fixt.start)) > 0)
      }
    }
    if (fs.contains("Print")) {
      assert(grammar(fixt.start).toString() == fixt.text)
    }
    if (fs.contains("Nullable")) {
      assert(grammar(fixt.start).nullable(grammar) == fixt.nullable)
    }
    if (fs.contains("First")) {
      assert(grammar(fixt.start).first(grammar) == fixt.first)
    }
  } 
  
  def makeAlgebra(fs: Set[String], trace: Writer): GrammarAlg[All,All] = {
     var cur: Any = null;
     val fss = fs.toList.sorted
     if (fss.contains("Nullable") && cur == null) {
       cur = decorate(grammarNullable[Nullable], new CircNullable)
     }
     
     if (fss.contains("Parse") && cur == null) {
       cur = decorate(grammarParse[Parse], new Memo)
       
       // tracing should be innermost
       if (fss.contains("Trace")) {
         cur = decorate(cur.asInstanceOf[OpenGrammarAlg[Parse,Parse]], new Trace(trace))
       }
     }
     if (fss.contains("Print") && cur == null) {
       cur = grammarPrint[Print]
     }
     
     if (fss.contains("Nullable") && fss.contains("Parse")) {
       val p: OpenGrammarAlg[All, Parse] = if (fss.contains("Trace")) {
         decorate(decorate(grammarParse[All], new Memo), new Trace(trace))
       }
       else {
         decorate(grammarParse[All], new Memo)
       }
       cur = combine[Nullable, Parse, All](
               cur.asInstanceOf[OpenGrammarAlg[All, Nullable]], p)
     }
     
     if (fss.contains("Nullable") && fss.contains("Print")) {
       cur = combine[Print, Nullable, All](
          grammarPrint[All],
          cur.asInstanceOf[OpenGrammarAlg[All, Nullable]]
          )
     }
     
     if (fss.contains("Nullable") && fss.contains("First")) {
       cur = combine[Nullable, First, All](
          cur.asInstanceOf[OpenGrammarAlg[All, Nullable]],
          decorate(grammarFirst[All], new CircFirst))
     }
      
     if (fss.contains("Parse") && fss.contains("Profile")) { 
       cur = merge[Parse, Profile, All](
           LiftParseProfileAll, // specific lifter
           cur.asInstanceOf[OpenGrammarAlg[All, Parse]],
           grammarProfile[All])
     }
     
     if (fss.contains("Parse") && fss.contains("Print")) { 
        cur = combine[Print, Parse, All](
            grammarPrint[All],
            cur.asInstanceOf[OpenGrammarAlg[All, Parse]]
            )
     }

     if (cur == null) {
       throw new RuntimeException("Could not make: " + fs)
     }
     return fclose(cur.asInstanceOf[OpenGrammarAlg[All, All]]);
  }
  
  
  case class AllConfFixture(start: String,
		  	src: Seq[String],
		  	parsed: Boolean, 
		  	text: String, 
		  	trace: Writer,
		  	nullable: Boolean, 
		  	first: Set[String]) 
  
  
  def iterAFixture(): AllConfFixture = new AllConfFixture(
         "A",
         as,
         true,
         "()|(\"a\" A)",
         new StringWriter,
         true,
         Set("a"))

  def leftRecFixture(): AllConfFixture = new AllConfFixture(
         "Exp",
         exp,
         true,
         "(Exp \"+\" Exp)|((Exp \"*\" Exp)|(((\"?\")? Exp)|(\"x\")))",
         new StringWriter,
         false,
         Set("?", "x")) 
  
  def testCombination[X](maker: GrammarAlg[X, X] => Map[String,X], comb: Set[String], fixt: AllConfFixture) {
    val alg = makeAlgebra(comb, fixt.trace)
    val grammar = maker(alg.asInstanceOf[GrammarAlg[X,X]]);
    runGrammar(comb, grammar.asInstanceOf[Map[String,All]], fixt)
  }
		  	
  test("All valid configurations") {
     val fs = Set("Parse", "Print", "Profile", "Trace", "Nullable", "First")
     val combs = powerset[String](fs).filter(isValid);
     for (comb <- combs) {
       testCombination(makeIterA, comb, iterAFixture())
       testCombination(makeLeftRec, comb, leftRecFixture())
     }
   }
  
  

  
  
}