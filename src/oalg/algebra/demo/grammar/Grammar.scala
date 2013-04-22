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

import oalg.algebra.core.Algebras.AlgebraDefault
import oalg.algebra.core.Algebras.Open
import oalg.algebra.core.Algebras.SelfFAlg
import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList
import java.io.Writer

object Grammar {
  
  trait GrammarAlg[In, Out] {
	def Alt(lhs: In, rhs: In): Out
	def Seq(lhs: In, rhs: In): Out
	def Opt(arg: In): Out
	def Terminal(word: String): Out
	def NonTerminal(name: String): Out
	def Empty(): Out
  }

  trait SelfGrammarAlg[S <: E, E] extends 
     GrammarAlg[S,Open[S, E]] with SelfFAlg[GrammarAlg,S,E]  
  
  type OpenGrammarAlg[S <: E, E] = (=> GrammarAlg[S,S]) => GrammarAlg[S,Open[S, E]]
  
  object GrammarComb extends AlgebraDefault[GrammarAlg] // combinators

  trait Parse {
	def parse(grammar: Map[String, Parse], input: Seq[String], cnt: Seq[String] => Unit): Unit
  }
  
  trait GrammarDesugarAlg[S <: E, E] extends SelfGrammarAlg[S, E] {
    def Opt(x: S) = oself => fself.Alt(x, fself.Empty)
  }

  /*
   * Recognition algorithm based on Mark Johnson, "Memoization in top-down parsing",
   * Computational Linguistics, Volume 21 Issue 3, September 1995, Pages 405-417 
   */
  
  trait GrammarParse[S <: Parse] extends GrammarDesugarAlg[S, Parse] {
    type G = Map[String, Parse]
  
    def Alt(lhs: S, rhs: S) =  self => new Parse {
	  def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit): Unit = {
		lhs.parse(g, input, cnt)
		rhs.parse(g, input, cnt)
	  }
    }
  
    def Seq(lhs: S, rhs: S) = self => new Parse {
      def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit) =
        lhs.parse(g, input, input2 => rhs.parse(g, input2, cnt))
    }
    
    def Terminal(word: String) = self => new Parse {
      def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit) =
        if (!input.isEmpty && input.head == word) 
          cnt(input.tail)
    }

    def NonTerminal(name: String) = self => new Parse {
      def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit) = 
        g(name).parse(g, input, cnt)
    }
  
    def Empty() = self => new Parse {
      def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit) = cnt(input)
    }
    
  }
  
  def grammarParse[S <: Parse] : OpenGrammarAlg[S, Parse] = 
    s => new GrammarParse[S] { lazy val fself = s }
  

  trait Nullable {
   def nullable(g: Map[String, Nullable]): Boolean
  }

  trait GrammarNullable[S <: Nullable] extends GrammarDesugarAlg[S, Nullable] {
    type G = Map[String, Nullable]
  
    def Alt(lhs: S, rhs: S) = self => new Nullable {
        def nullable(g: G) = lhs.nullable(g) || rhs.nullable(g) 
      }
  
    def Seq(lhs: S, rhs: S) = self => new Nullable {
        def nullable(g: G) = lhs.nullable(g) && rhs.nullable(g) 
      }
  
    def Terminal(word: String) = self => new Nullable {
        def nullable(g: G) = false 
      }

    def NonTerminal(name: String) = self => new Nullable {
        def nullable(g: G) = g(name).nullable(g) 
      }
  
    def Empty() = self => new Nullable {
        def nullable(g: G) = true 
      }
  }
  
  def grammarNullable[S <: Nullable]: OpenGrammarAlg[S, Nullable] = 
    s => new GrammarNullable[S] { lazy val fself = s }
  
  
  trait First {
    def first(g: Map[String, Nullable with First]): Set[String]
  }

  trait GrammarFirst[S <: Nullable with First] extends GrammarDesugarAlg[S, First] {
    type G = Map[String, Nullable with First]
  
    def Alt(lhs: S, rhs: S) =  self => new First {
      def first(g: G): Set[String] = lhs.first(g) ++ rhs.first(g) 
    }
  
    def Seq(lhs: S, rhs: S) = self => new First {
      def first(g: G) = if (lhs.nullable(g)) lhs.first(g) ++ rhs.first(g) else lhs.first(g) 
    }
  
    def Terminal(word: String) = self => new First {
      def first(g: G) = Set(word) 
    }

    def NonTerminal(name: String) = self => new First {
      def first(g: G) = g(name).first(g) 
    }
  
    def Empty() = self => new First {
      def first(g: G) = Set() 
    }
  }
  
  def grammarFirst[S <: Nullable with First]: OpenGrammarAlg[S, First] =
    s => new GrammarFirst[S] { lazy val fself = s }
    
    
  trait Print {
    def toString: String
  }
    
  trait GrammarPrint[S <: Print] extends SelfGrammarAlg[S, Print] {
    def Alt(lhs: S, rhs: S) = self => new Print {
      override def toString() = "(" + lhs + ")|(" + rhs + ")" 
    }
  
    def Seq(lhs: S, rhs: S) = self => new Print {
      override def toString() = lhs + " " + rhs 
    }
    
    def Opt(arg: S) = self => new Print {
      override def toString() = "(" + arg + ")?"
    }
  
    def Terminal(word: String) = self => new Print {
      override def toString() = "\"" + word + "\"" 
    }

    def NonTerminal(name: String) = self => new Print {
      override def toString() = name 
    }
  
    def Empty() = self => new Print {
      override def toString() = "" 
    }
  }

  def grammarPrint[S <: Print]: OpenGrammarAlg[S, Print] =
    s => new GrammarPrint[S] { lazy val fself = s }
    
  trait Profile {
    def profile: Map[Profile, Integer]
  }

    
  trait GrammarProfile[S <: Profile] extends GrammarDesugarAlg[S, Profile] {
    private def combine[X](map1: Map[X, Integer], map2: Map[X, Integer]): Map[X, Integer] =
      map1 ++ map2.map{ case (k,v) => k -> (v + map1.getOrElse(k,0:Integer)) }.asInstanceOf[Map[X, Integer]] // why the cast?
  
    def Alt(lhs: S, rhs: S) = self => new Profile {
      def profile: Map[Profile, Integer] =  combine[Profile](lhs.profile, combine[Profile](rhs.profile, Map(self -> 1)))
    }
  
    def Seq(lhs: S, rhs: S) = self => new Profile {
      def profile: Map[Profile,Integer] = combine[Profile](lhs.profile, combine[Profile](rhs.profile, Map(self -> 1))) 
    }
  
    def Terminal(word: String) = self => new Profile {
      def profile: Map[Profile, Integer] = Map(self -> 1) 
    }

    def NonTerminal(name: String) = self => new Profile {
      def profile: Map[Profile, Integer] = Map(self -> 1) 
    }
  
    def Empty() = self => new Profile {
      def profile: Map[Profile, Integer] = Map(self -> 1) 
    }
  }
  
  def grammarProfile[S <: Profile]: OpenGrammarAlg[S, Profile] =
    s => new GrammarProfile[S] { lazy val fself = s }
    
  class Memo extends (Parse => Parse) {
    type G = Map[String, Parse]
    class Entry {
      val cnts: MutableList[Seq[String] => Unit] = new MutableList[Seq[String] => Unit]
      val results: MutableList[Seq[String]] = new MutableList[Seq[String]]
      def isSubsumed(input: Seq[String]): Boolean = results.contains(input)
    }
  
    override def apply(p: Parse): Parse = {
      val table = new HashMap[Seq[String], Entry]
      return new Parse {
        override def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit): Unit = {
          if (!table.contains(input)) {
            table.put(input, new Entry())
          }
          val entry = table(input)
          if (entry.cnts.isEmpty) {
			entry.cnts += cnt;
			p.parse(g, input, inp => { 
			  if (!entry.isSubsumed(inp)) {
				entry.results += inp;
				for (c <- entry.cnts) c(inp)
			  }
			});
		  }
		  else {
			entry.cnts += cnt;
			for (r <- entry.results) cnt(r)
		  }
        }
      }
    }
  }  
  
  class Trace(w: Writer) extends (Parse => Parse) {
    type G = Map[String, Parse]
    var indent = 0;
    override def apply(p: Parse): Parse = new Parse {
	  def parse(g: G, input: Seq[String], cnt: Seq[String] => Unit): Unit = {
	    w.write("[")
	    p.parse(g, input, cnt)
	    w.write("]")
	    w.flush();
	  }
    }
  }
  
  class CircNullable extends (Nullable => Nullable) {
    import oalg.algebra.aspects.Circ
    type G = Map[String, Nullable]
    override def apply(n: Nullable): Nullable = {
      val sup = new Circ[Boolean, G](false, n.nullable _)
      new Nullable {
        def nullable(g: G) = sup(g)
      }
    }
  }
  
  class CircFirst extends (First => First) {
    import oalg.algebra.aspects.Circ
    type G = Map[String, Nullable with First]
    override def apply(f: First): First = {
      val sup = new Circ[Set[String], G](Set(), f.first _)
      new First {
        def first(g: G) = sup(g)
      }
    }
  }
}