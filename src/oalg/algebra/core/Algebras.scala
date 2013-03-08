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

package oalg.algebra.core

object Algebras {
  import scala.reflect._
  trait Lifter[A,B, S <: A with B] {
    def lift(x : A, y : B) : Open[S, A with B]
  }
  type Open[S <: E, E] = (=> S) => E
  type OFAlg[F[_,_],S <: E, E] = F[S, Open[S,E]]
  type FAlg[F[_,_],E] = F[E,E]
  
  trait SelfFAlg[F[_,_],Self <: Exp, Exp] {
    val fself : F[Self, Self]
  }
  
  class LiftDecorate[S <: A, A](action : A => A) extends Lifter[A,Any,S] {
    def lift(x : A, y : Any) = self => action(x)
  }

  class MkLifter[A,B, S <: A with B](f : (A,B) => A with B) extends Lifter[A,B,S] {
    def lift(x : A, y : B) : Open[S,A with B] = self => f(x,y)
  }
  
  trait Algebra[F[_,_]] {
    type FOpen[F[_,_],S <: T, T] = (=> F[S,S]) => F[S,Open[S,T]]
     
    // Basic combinators
    def merge[A,B,S <: A with B](mix : Lifter[A,B,S], a1 : FOpen[F,S,A], a2 : FOpen[F,S,B]) : FOpen[F,S,A with B] 
    def empty[S] : FOpen[F,S,Any] 
  
    // Derived combinator(s)
    def decorate[A, S <: A](parent : FOpen[F,S,A], action : A => A) : FOpen[F,S,A] =
      merge[A,Any,S](new LiftDecorate(action),parent,empty[S])
      
    // Closing combinators
    def fcloseAlg[S](a : F[S,Open[S,S]]) : F[S,S]
    def fclose[S](f : FOpen[F,S,S]) : F[S,S]
  }
  
  
  def createInstance[A](ih : java.lang.reflect.InvocationHandler)(implicit m : ClassTag[A]) : A = {
      java.lang.reflect.Proxy.newProxyInstance(m.runtimeClass.getClassLoader, Array(m.runtimeClass),ih).asInstanceOf[A]
  }
  
  def delegate[A,B, S <: A with B](x : A, y : B)(implicit m : ClassTag[S]) : S = createInstance[S](new java.lang.reflect.InvocationHandler() {
      def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
	      try {
	        method.invoke(x, args : _*)
	      } catch {
	        case e : IllegalArgumentException => method.invoke(y, args : _*)
	      }
	    }
    })
  
    
  trait AlgebraDefault[F[_,_]] {
    type FOpen[F[_,_],S <: T, T] = (=> F[S,S]) => F[S,Open[S,T]]
     
    def merge[A,B,S <: A with B](mix : Lifter[A,B,S], a1 : FOpen[F,S,A], a2 : FOpen[F,S,B]) (implicit m : ClassTag[F[S,Open[S,A with B]]]) : FOpen[F,S,A with B] = 
      s => createInstance[F[S, Open[S,A with B]]](new java.lang.reflect.InvocationHandler() {
	    def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = (me : Function0[S]) => {
	      val a = method.invoke(a1(s),args : _*) 
	      val b = method.invoke(a2(s),args : _*)
	      mix.lift(a.asInstanceOf[Open[S,A]](me()),b.asInstanceOf[Open[S,B]](me()))(me()).asInstanceOf[Object]
	    }
      })
      
    def combine[A, B, S <: A with B](alg1 : FOpen[F, S, A], alg2 : FOpen[F, S, B])
        (implicit 
            m0 : ClassTag[S],
            m1 : ClassTag[FOpen[F, S, A with B]], 
            m2 : ClassTag[F[S, Open[S,A with B]]]) : FOpen[F, S, A with B] = {
      merge[A,B,S](new MkLifter[A, B, S](delegate[A,B,S] _), alg1, alg2)
    }
      
    def empty[S](implicit m : ClassTag[F[S,Open[S,Any]]]) : FOpen[F,S,Any] = s =>
      createInstance[F[S,Open[S,Any]]](new java.lang.reflect.InvocationHandler() {
	    def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) = ((s : Any) => new Object())
	  })

      
    def fcloseAlg[S](a : F[S,Open[S,S]])(implicit m : ClassTag[F[S,S]]) : F[S,S] = 
      createInstance[F[S,S]](new java.lang.reflect.InvocationHandler() {
	    def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) = {
	      lazy val s : S = method.invoke(a, args : _*).asInstanceOf[Open[S,S]](s)
	      s.asInstanceOf[Object]
	    }
	  })
	  
	def fclose[S](f : FOpen[F,S,S])(implicit m : ClassTag[F[S,S]]) : F[S,S] = {
      lazy val c : F[S,S] = fcloseAlg(f(c))
	  c
    }

    def decorate[A, S <: A](parent : FOpen[F,S,A], action : A => A)(implicit m1 : ClassTag[F[S,Open[S,A]]], m2 : ClassTag[F[S,Open[S,Any]]]) : FOpen[F,S,A] =
      merge[A,Any,S](new LiftDecorate(action),parent,empty[S])
  }
  
}