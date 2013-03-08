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
package oalg.algebra.demo.stacks

import oalg.algebra.demo.stacks.Stacks._
import oalg.algebra.core.Algebras.Lifter

trait DStack extends Stack {
  val st: Stack
  def empty() = st.empty
  def push(s : Char) { st.push(s) }
  def push2(s : Char){ st.push2(s) }
  def pop() { st.pop }
  def top() = st.top
}

trait DCounter extends Counter {
  val ct: Counter
  def reset() { ct.reset }
  def inc() { ct.inc }
  def dec() { ct.dec }
  def size() = ct.size()
}
  
trait DLock extends Lock {
  val lk: Lock
  def lock() { lk.lock }
  def unlock() { lk.unlock }
  def is_unlocked() = lk.is_unlocked
}
  
trait DBound extends Bound {
  val bd: Bound
  def check(s: Char) = bd.check(s)
}
  
class StackWithCounter(self: => Stack with Counter, s: Stack, c: Counter)
  extends DStack with DCounter {
  val st = s; val ct = c
  override def empty() {self.reset; st.empty}        
  override def push(s : Char) {self.inc; st.push(s)} 
  override def pop() {self.dec; st.pop}              
}

object LiftSCF extends Lifter[Stack,Counter, Stack with Counter] {
  def lift(st : Stack, ct : Counter) = 
    self => new StackWithCounter(self, st, ct)  
}

object LiftSCF2 extends Lifter[Stack,Counter, Stack with Counter with Bound] {
  def lift(st : Stack, ct : Counter) = 
    self => new StackWithCounter(self, st, ct)  
}
  
object LiftSCF3 extends Lifter[Stack,Counter, Stack with Counter with Lock] {
  def lift(st : Stack, ct : Counter) = 
    self => new StackWithCounter(self, st, ct)  
}
  
object LiftSCF4 extends Lifter[Stack,Counter, Stack with Counter with Lock with Bound] {
  def lift(st : Stack, ct : Counter) = 
    self => new StackWithCounter(self, st, ct)  
}

class StackWithLock(self: => Stack with Lock, s: Stack, l: Lock) extends DStack with DLock {
  val st = s
  val lk = l
  override def empty() {if (self.is_unlocked) st.empty}          
  override def push(s : Char) {if (self.is_unlocked) st.push(s)} 
  override def pop() {if (self.is_unlocked) st.pop}              
}
  
object LiftSLF extends Lifter[Stack, Lock, Stack with Lock] {
  def lift(st : Stack, ct : Lock) = self => new StackWithLock(self, st, ct)
}

object LiftSLF2 extends Lifter[Stack, Lock, Stack with Lock with Bound] {
  def lift(st : Stack, ct : Lock) = self => new StackWithLock(self, st, ct)
}
  
class CounterWithLock(self: => Lock with Counter, c: Counter, l: Lock) extends DCounter with DLock {
  val ct = c
  val lk = l
  override def reset() { if (self.is_unlocked) ct.reset }
  override def inc() { if (self.is_unlocked) ct.inc }
  override def dec() { if (self.is_unlocked) ct.dec }
}
  
object LiftCLF extends Lifter[Counter, Lock, Counter with Lock] {
  def lift(c: Counter, l: Lock) = self => new CounterWithLock(self, c, l)
}
  
class StackWithCounterWithLock(self: => Stack with Counter with Lock, 
                                 sc: Stack with Counter, l: Lock)
  extends StackWithLock(self, sc, l) with DCounter {
  val ct = sc
}
  
object LiftSCLF extends Lifter[Stack with Counter, Lock, Stack with Counter with Lock] {
  def lift(st : Stack with Counter, lk : Lock) = self => 
    new StackWithCounterWithLock(self, st, lk)
}

object LiftSCLF2 extends Lifter[Stack with Counter, Lock, Stack with Counter with Lock with Bound] {
  def lift(st : Stack with Counter, lk : Lock) = self => 
    new StackWithCounterWithLock(self, st, lk)
}
  
class StackWithBound(self: => Stack with Bound, s: Stack, b: Bound)  extends DStack with DBound {
  val st = s
  val bd = b
  override def push(s : Char) { if (bd.check(s)) st.push(s) else println("Not a letter: " + s) } 
}
  
object LiftSBF extends Lifter[Stack,Bound, Stack with Bound] {
  def lift(st : Stack, bd : Bound) = self => new StackWithBound(self, st, bd)  
}
  
class StackWithCounterWithLockWithBound(self: => Stack with Counter with Lock with Bound, 
  s: Stack with Counter with Lock, b: Bound)  
  extends StackWithBound(self, s, b) with DCounter with DLock {
  val ct = s
  val lk = s
}

object LiftSCLBF extends Lifter[Stack with Counter with Lock, Bound, Stack with Counter with Lock with Bound] {
  def lift(st : Stack with Counter with Lock, bd : Bound) = self => 
    new StackWithCounterWithLockWithBound(self, st, bd)  
}

class StackWithCounterWithBound(self: => Stack with Counter with Bound, s: Stack with Counter, b: Bound) 
  extends StackWithBound(self, s, b) with DCounter {
  val ct = s
}
  
object LiftSCBF extends Lifter[Stack with Counter, Bound, Stack with Counter with Bound] {
  def lift(st : Stack with Counter, bd : Bound) = self => 
    new StackWithCounterWithBound(self, st, bd)  
}
  
class StackWithLockWithBound(self: => Stack with Lock with Bound, s: Stack with Lock, b: Bound) 
  extends StackWithBound(self, s, b) with DLock {
  val lk = s
}
  
object LiftSLBF extends Lifter[Stack with Lock, Bound, Stack with Lock with Bound] {
  def lift(st : Stack with Lock, bd : Bound) = self => 
  new StackWithLockWithBound(self, st, bd)  
}


object Main extends App {
  
  def stack[S](f : StackAlg[S,S]) : S = f.stack()
  def counter[S](f: CounterAlg[S,S]): S = f.counter()
  
  override def main(args: Array[String]) {
    println(">>> Stack")
    testSF()
    println(">>> Stack + Counter")
    testSCF()
    println(">>> Stack + Lock")
    testSLF()
    println(">>> Stack + Bound")
    testSBF()
    println(">>> Stack + Lock + Bound")
    testSLBF()

    println(">>> Stack + Counter + Bound")
    testSCBF()

    println(">>> Stack + Counter + Lock")
    testAll()
    println(">>> Stack + Counter + Lock + Bound")
    testMore()
    
    println(">>> Counter")
    testCounter();
    println(">>> Counter + Lock")
    testCounterLock()
  }
  
  def testCounter() {
    import CounterComb._
    val o = fclose(cfc[Counter])
    val c = counter(o)
    c.inc
    c.inc
    println(c.size)
    c.dec
    println(c.size)
  }
  
  def testCounterLock() {
    import CounterComb._
    val o = fclose(merge[Counter, Lock, Counter with Lock](LiftCLF, cfc, lfc))
    val c = counter(o)
    c.inc
    c.inc
    println(c.size)
    println("LOCKING")
    c.lock
    c.inc
    c.inc
    println(c.is_unlocked)
    println("UNLOCK")
    c.unlock
    c.inc
    println(c.size)
    c.dec
    println(c.size)
  }
  
  def testSBF() = {
    import StackComb._ 
   
    val o : Stack with Bound = 
      fclose(merge[Stack, Bound, Stack with Bound](LiftSBF, sf, bf)).stack()
    o.empty()
    o.push('m')
    o.push('a')
    o.push('i')
    o.push('2') // rejected
    o.push2('l')
    o.push('i')
    o.push('W')
    
    for (_ <- 1 to 7) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
  def testSLBF() = {
    import StackComb._ 
   
    val o : Stack with Lock with Bound = 
      fclose(merge[Stack with Lock, Bound, Stack with Lock with Bound](LiftSLBF, 
          merge[Stack, Lock, Stack with Lock with Bound](LiftSLF2, sf, lf), bf)).stack()
    o.empty()
    println("Unlocked: " + o.is_unlocked)
    //END_SCL_CLIENT
    o.push('m')
    println("Unlocked: " + o.is_unlocked)
    o.push('a')
    println("Unlocked: " + o.is_unlocked)
    println("LOCKING")
    o.lock()
    o.push('i')
    println("Unlocked: " + o.is_unlocked)
    o.push2('l')
    o.push('2') // rejected
    println("Unlocked: " + o.is_unlocked)
    println("UNLOCKING")
    o.unlock()
    o.push('i')
    println("Unlocked: " + o.is_unlocked)
    o.push('W')
    println("Unlocked: " + o.is_unlocked)
    
    for (_ <- 1 to 4) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  def testSLF() = {
    import StackComb._ 
   
    val o : Stack with Lock = 
      fclose(merge[Stack, Lock, Stack with Lock](LiftSLF, sf, lf)).stack()
    o.empty()
    println("Unlocked: " + o.is_unlocked)
    //END_SCL_CLIENT
    o.push('m')
    println("Unlocked: " + o.is_unlocked)
    o.push('a')
    println("Unlocked: " + o.is_unlocked)
    println("LOCKING")
    o.lock()
    o.push('i')
    println("Unlocked: " + o.is_unlocked)
    o.push2('l')
    println("Unlocked: " + o.is_unlocked)
    println("UNLOCKING")
    o.unlock()
    o.push('i')
    println("Unlocked: " + o.is_unlocked)
    o.push('W')
    println("Unlocked: " + o.is_unlocked)
    
    for (_ <- 1 to 4) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
  // SF configuration
  def testSF() = {
    import StackComb._ 
    
    val o = stack[Stack](fclose(sf))
    o.empty()
    o.push('m')
    o.push('a')
    o.push('i')
    o.push2('l')
    o.push('i')
    o.push('W')
    
    for (_ <- 1 to 7) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
  // SF + CF configuration
  def testSCF() = {
    import StackComb._     
    val o : Stack with Counter = 
       fclose(merge[Stack, Counter, Stack with Counter](
           LiftSCF, sf, cf)).stack()
    println("Size: " + o.size())
    o.push('m')
    println("Size: " + o.size())
    o.push('a')
    println("Size: " + o.size())
    o.push('i')
    println("Size: " + o.size())
    o.push2('l')
    println("Size: " + o.size())
    o.push('i')
    println("Size: " + o.size())
    o.push('W')
    println("Size: " + o.size())
    
    for (_ <- 1 to 7) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
   // SF + CF + BF configuration
  def testSCBF() = {
    import StackComb._     
    val o : Stack with Counter with Bound =
      fclose(merge[Stack with Counter, Bound, Stack with Counter with Bound](LiftSCBF, 
          merge[Stack, Counter, Stack with Counter with Bound](LiftSCF2, sf, cf), bf)).stack()
    println("Size: " + o.size())
    o.push('m')
    println("Size: " + o.size())
    o.push('a')
    println("Size: " + o.size())
    o.push('i')
    println("Size: " + o.size())
    o.push2('l')
    println("Size: " + o.size())
    o.push('i')
    o.push('2') // rejected
    println("Size: " + o.size())
    o.push('W')
    println("Size: " + o.size())
    
    for (_ <- 1 to 7) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  // SF + CF + LF
  def testAll() = {
    import StackComb._ 
   
    val o : Stack with Counter with Lock = 
      fclose(merge[Stack with Counter,Lock,Stack with Counter with Lock](LiftSCLF,
          merge[Stack,Counter,Stack with Counter with Lock](LiftSCF3,sf,cf),lf)).stack()
    o.empty()
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('m')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('a')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    println("LOCKING")
    o.lock()
    o.push('i')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push2('l')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    println("UNLOCKING")
    o.unlock()
    o.push('i')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('W')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    
    for (_ <- 1 to 4) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
   // SF + CF + LF + BF
  def testMore() = {
    import StackComb._ 
   
    
    val o : Stack with Counter with Lock with Bound  = 
      stack(fclose(
        merge[Stack with Counter with Lock, Bound, Stack with Counter with Lock with Bound](LiftSCLBF,
          merge[Stack with Counter,Lock,Stack with Counter with Lock with Bound](LiftSCLF2,
            merge[Stack,Counter,Stack with Counter with Lock with Bound](LiftSCF4,
                sf,
                cf),
            lf
          ), 
          bf
       )))
    o.empty()
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('m')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('a')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    println("LOCKING")
    o.lock()
    o.push('i')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push2('l')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    println("UNLOCKING")
    o.unlock()
    o.push('i')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('W')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    o.push('2')
    println("Size: " + o.size())
    println("Unlocked: " + o.is_unlocked)
    for (_ <- 1 to 4) {
      print(o.top())
      o.pop()
    }
   
    print("\n")
  }
  
  
}