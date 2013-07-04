/*******************************************************************************
 * Copyright (c) 2012-2013
 * - Bruno C.d.S. Oliveira (oliveira@comp.nus.edu.sg)
 * - Tijs van der Storm (storm@cwi.nl)
 * - Alex Loh (alexloh@cs.utexas.edu)
 * - William R. Cook (wcook@cs.utexas.edu)
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
********************************************************************************/

package oalg.algebra.demo.grammar

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
import java.io.PrintWriter

class ParseWithProfile(self: => Parse with Profile, pa: Parse, pr: Profile) extends Parse with Profile {
  type G = Map[String, Parse]
  var count: Integer = 0
     
  def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit): Unit = {
    count += 1
    pa.parse(g, input, cnt)
  }
	  
  def profile = pr.profile ++ Map(self -> count)
}
  
object LiftParseProfile extends Lifter[Parse, Profile, PPP] {
  type G = Map[String, Parse]
  def lift(pa: Parse, pr: Profile) = self => new ParseWithProfile(self, pa, pr)
}

object LiftParseProfileAll extends Lifter[Parse, Profile, All] {
  type G = Map[String, Parse]
  def lift(pa: Parse, pr: Profile) = self => new ParseWithProfile(self, pa, pr)
}

class ParseWithProfileWithPrint(self: => Parse with Profile with Print, pp: Parse with Profile, p: Print)
  extends ParseWithProfile(self, pp, pp) with Print {
  override def toString = p.toString()
} 
  
object LiftParseProfilePrint extends Lifter[Parse with Profile, Print, PPP] {
  type G = Map[String, Parse]
  def lift(pa: Parse with Profile, pr: Print) = self => new ParseWithProfileWithPrint(self, pa, pr)
}

// Nominal types to make proxies work.
trait NF extends Nullable with First
trait PN extends Parse with Nullable
trait PPP extends Parse with Profile with Print
trait PNF extends Parse with Nullable with First
trait All extends Parse with Print with Nullable with Profile with First
    
  
object Main extends App {
  
  def makeIterA[X](f: GrammarAlg[X, X]): Map[String,X] = {
    // A ::= | "a" A
    val e = f.Empty
    val a = f.NonTerminal("A")
    val w = f.Terminal("a")
    Map("A" -> f.Alt(e, f.Seq(w, a)))
  }
  
  def makeLeftRec[X](f: GrammarAlg[X, X]): Map[String, X] = {
    // Exp ::= Exp "+" Exp | "x"
    val exp = f.NonTerminal("Exp")
    val plus = f.Terminal("+")
    val star = f.Terminal("*")
    val add = f.Seq(exp, f.Seq(plus, exp))
    val mul = f.Seq(exp, f.Seq(star, exp))
    val opt = f.Seq(f.Opt(f.Terminal("?")), exp)
    val x = f.Terminal("x")
    Map("Exp" -> f.Alt(add, f.Alt(mul, f.Alt(opt, x))))
  }
  
  
  def testParse[X <: Parse](maker: GrammarAlg[X, X] => Map[String,X], alg: GrammarAlg[X, X], start: String, input: Seq[String]): X = {
    val g = maker(alg)
    parse(g, g(start), input)
    g(start)
  }
  
  def parse(g: Map[String,Parse], p: Parse, input: Seq[String]): Unit = {
    try {
      p.parse(g, input, rest => {
        if (rest.isEmpty) {
          System.out.println("Success")
          throw new RuntimeException("Success")
        }
        else {
          System.out.println("Success, remaining: " + rest)
        }
      })
    }
    catch {
      case e:RuntimeException => return
    }
    System.out.println("Failed")
  }
  
  def printProfile(prof: Map[Profile,Integer]): Unit = {
    for ((x, n) <- prof) {
      println(f"$x%50s $n%d")
    }
  }
  
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
  
  def powerset[A](s: Set[A]) = s.foldLeft(Set(Set.empty[A])) { case (ss, el) => ss ++ ss.map(_ + el) }
  
  def generateAll() {
    val fs = Set("Parse", "Print", "Profile", "Trace", "Nullable", "First")
    val combs = powerset[String](fs).filter(isValid);
    println("BEGIN!!!!")
    for (s <- combs) {
      println("******************* " + s)
      val alg = makeAlgebra(s)
      try {
        runAlg(s, alg)
      }
      catch {
        case e:Exception => {
          println("--------------------------------> Exception:" + e)
//          println(e.getStackTraceString)
          throw e
        }
      }
    }
    println(combs.size)
  }

 
  def makeAlgebra(fs: Set[String]): GrammarAlg[All,All] = {
     var cur: Any = null;
     val fss = fs.toList.sorted
     if (fss.contains("Nullable") && cur == null) {
       cur = decorate(grammarNullable[Nullable], new CircNullable)
     }
     if (fss.contains("Parse") && cur == null) {
       cur = decorate(grammarParse[Parse], new Memo)
       // tracing should be innermost
       if (fss.contains("Trace")) {
         val out = new PrintWriter(System.out)
         cur = decorate(cur.asInstanceOf[OpenGrammarAlg[Parse,Parse]], new Trace(out))
       }
     }
     if (fss.contains("Print") && cur == null) {
       cur = grammarPrint[Print]
     }
     
     if (fss.contains("Nullable") && fss.contains("Parse")) {
       val p: OpenGrammarAlg[All, Parse] = if (fss.contains("Trace")) {
         decorate(decorate(grammarParse[All], new Memo), new Trace(new PrintWriter(System.out)))
       }
       else {
         decorate(grammarParse[All], new Memo)
       }
       cur = combine[Nullable, Parse, All](
           cur.asInstanceOf[OpenGrammarAlg[All, Nullable]], p)
     }
     
     if (fss.contains("Nullable") && fss.contains("Print")) {
       cur = combine[Nullable, Print, All](
          cur.asInstanceOf[OpenGrammarAlg[All, Nullable]],
          grammarPrint[All])
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
        cur = combine[Parse, Print, All](
            cur.asInstanceOf[OpenGrammarAlg[All, Parse]],
            grammarPrint[All])
     }

     
  
     if (cur == null) {
       throw new RuntimeException("Could not make: " + fs)
     }
     return fclose(cur.asInstanceOf[OpenGrammarAlg[All, All]]);
  }
  
  
  def runAlg(fs: Set[String], alg: GrammarAlg[All,All]) {
    if (fs.contains("Parse")) {
      println(">>>> Parsing")
      val as = Seq("a", "a", "b")
      val exp = Seq("x", "+", "x", "+", "x", "*", "x")
      val p1 = testParse(makeIterA[Parse], alg.asInstanceOf[GrammarAlg[Parse,Parse]], "A", as)
      val p2 = testParse(makeLeftRec[Parse], alg.asInstanceOf[GrammarAlg[Parse,Parse]], "Exp", exp)
      if (fs.contains("Profile")) {
        println(">>>> Profile")
        printProfile(p1.asInstanceOf[Profile].profile)
        printProfile(p2.asInstanceOf[Profile].profile)
      }
    }
    if (fs.contains("Print")) {
      println(">>>> Printing")
      val x = makeIterA[Print](alg.asInstanceOf[GrammarAlg[Print,Print]])
      val y = makeLeftRec[Print](alg.asInstanceOf[GrammarAlg[Print,Print]])
      println(x)
      println(y)
    }
    if (fs.contains("Nullable")) {
      println(">>>> Nullable")
      val g = alg.asInstanceOf[GrammarAlg[Nullable,Nullable]]
      val x = makeIterA[Nullable](g)
      val y = makeLeftRec[Nullable](g)
      println(x("A").nullable(x))
      println(y("Exp").nullable(y))
    }
    if (fs.contains("First")) {
      println(">>>> First")
      val g = alg.asInstanceOf[GrammarAlg[Nullable with First, Nullable with First]]
      val x = makeIterA[Nullable with First](g)
      val y = makeLeftRec[Nullable with First](g)
      println(x("A").first(x))
      println(y("Exp").first(y))
    }
  }
  
  override def main(args: Array[String]) {
        
    println(">>>> Parsing")
    val as = Seq("a", "a", "b")
    val exp = Seq("x", "+", "x", "+", "x", "*", "x")
    testParse(makeIterA[Parse], fclose(grammarParse[Parse]), "A", as)
    testParse(makeLeftRec[Parse], fclose(decorate(grammarParse[Parse], new Memo)), "Exp", exp)
    
    
    println(">>>> Parsing with Tracing")
    val out = new PrintWriter(System.out)
    testParse(makeIterA[Parse], fclose(decorate(grammarParse[Parse], new Trace(out))), "A", as)
    testParse(makeLeftRec[Parse], fclose(decorate(decorate(grammarParse[Parse], new Memo), new Trace(out))), "Exp", exp)

    println(">>>> Parsing with Profiling and printing")
    
    
    val step1 = merge[Parse, Profile, PPP](
                  LiftParseProfile, // variance issue if not use PPP 
                  decorate(grammarParse[PPP], new Memo),
                  grammarProfile[PPP])

    val step2 = merge[Parse with Profile, Print, PPP](
    			  LiftParseProfilePrint, 
    			  step1, 
    			  grammarPrint[PPP])
    
    
    // fclose expects S, S not, e.g.,  A with B and AwithB
    val oap = fclose(step2.asInstanceOf[OpenGrammarAlg[PPP, PPP]]) 
    val gprof = makeLeftRec(oap)
    parse(gprof, gprof("Exp"), exp);
    printProfile(gprof("Exp").profile)
    
    println(">>>> NULLABLE Itera a")
    val g1 = makeIterA[Nullable](fclose(grammarNullable[Nullable]))
    println(g1("A").nullable(g1))

    println(">>>> NULLABLE left rec")
    val g2 = makeLeftRec[Nullable](fclose(decorate(grammarNullable[Nullable], new CircNullable)))
    println(g2("Exp").nullable(g2))

    println(">>>> First set")
    val gf1 = makeIterA(fclose(
        combine[Nullable, First, NF](
            decorate[Nullable, NF](grammarNullable[NF], new CircNullable),
            decorate[First, NF](grammarFirst[NF], new CircFirst)).asInstanceOf[OpenGrammarAlg[NF,NF]]))

    println(gf1("A").first(gf1))

    val gf2 = makeLeftRec(fclose(
        combine[Nullable, First, NF](
            decorate(grammarNullable[NF], new CircNullable),
            decorate(grammarFirst[NF], new CircFirst)).asInstanceOf[OpenGrammarAlg[NF,NF]]))

    println(gf2("Exp").first(gf2))

            
    println(">>>> Combining parsing and nullable")
    val oa = fclose(
      combine[Parse, Nullable, PN](
        decorate[Parse, PN](grammarParse[PN], new Memo),
        decorate[Nullable, PN](grammarNullable[PN], new CircNullable)).asInstanceOf[OpenGrammarAlg[PN,PN]])
    
    testParse(makeIterA[PN], oa, "A", as)
    testParse(makeLeftRec[PN], oa, "Exp", exp)
    val g3 = makeIterA(oa)
    println(g3("A").nullable(g3))

    val g4 = makeLeftRec(oa)
    println(g4("Exp").nullable(g4))
        
    
    
    println(gf2("Exp").first(gf2))

            
    println(">>>> Combining parsing and nullable and first")
    
    
    val oa1 = fclose(
        combine[Parse, Nullable with First, PNF](
        decorate(grammarParse[PNF], new Memo),
         combine[Nullable, First, PNF](
            decorate(grammarNullable[PNF], new CircNullable),
            decorate(grammarFirst[PNF], new CircFirst))).asInstanceOf[OpenGrammarAlg[PNF,PNF]])
    
    testParse(makeIterA[PNF], oa1, "A", as)
    testParse(makeLeftRec[PNF], oa1, "Exp", exp)
    val gfp3 = makeIterA(oa1)
    println(gfp3("A").nullable(gfp3))
    println(gfp3("A").first(gfp3))

    val gfp4 = makeLeftRec(oa1)
    println(gfp4("Exp").nullable(gfp4))
    println(gfp4("Exp").first(gfp4))
 
    println("================= 'ALL' configs ==========")
    generateAll();

    
    
    println("================= 'Paper' configs ==========")
    val f = fclose(
      combine[Parse, Nullable with First, PNF](
        decorate(grammarParse[PNF], new Memo),
        combine[Nullable, First, PNF](
          decorate(grammarNullable[PNF], new CircNullable),
          decorate(grammarFirst[PNF], new CircFirst))
        ).asInstanceOf[OpenGrammarAlg[PNF,PNF]])
        
    // A ::= | "a" A
    val g = Map("A" -> 
             f.Alt(f.Empty,f.Seq(f.Terminal("a"), f.NonTerminal("A"))))
    val s = g("A")
    s.parse(g, Seq("a", "a"), x => println("Yes"))
    println(s.first(g)) // -> {"a"}
    println(s.nullable(g)) // -> true
  }
  
}