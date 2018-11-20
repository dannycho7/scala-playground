package edu.ucsb.cs.cs162.regex.vm

import edu.ucsb.cs.cs162.regex.parse_tree._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

// A virtual machine that uses Thompson's powerset strategy to implement a
// non-backtracking algorithm for regular expression matching.
class PowersetVm(program: Program) extends VirtualMachine(program) {
  override def eval(str: String): Option[ParseTree] = {
    // Algorithm:
    // 1. compute initial set of threads (the ε-closure of the nfa start state)
    // 2. if the input string is empty go to step 7
    // 3. run the threads until they reach a match or accept instruction
    // 4. compact them to enforce at most one thread per program counter
    // 5. execute the surviving threads one step (i.e., the match or accept instruction)
    // 6. go to step 2
    // 7. compact the final set of threads
    // 8. if there is a surviving thread at an accept instruction, then that
    //    thread's 'parse' contains the final answer; otherwise there is no answer

    // Execute all given threads until they reach either a MatchSet or an Accept
    // instruction; returns the resulting set of Threads.
    @annotation.tailrec
    def runUntilMatchOrAccept(thread: Thread, todo: Set[Thread], result: Set[Thread]): Set[Thread] = {
      val Thread(pc, progress, priority, parse) = thread
      program(pc) match {
        case `Accept` | MatchSet(_) => {
          val next_result = result ++ Set(thread)
          if (todo.isEmpty) {
            next_result
          } else {
            runUntilMatchOrAccept(todo.head, todo.tail, next_result)
          }
        }
        case `Reject` => runUntilMatchOrAccept(todo.head, todo.tail, result)
        case `CheckProgress` => {
          if (thread.progress.contains(pc)) runUntilMatchOrAccept(todo.head, todo.tail, result)
          else runUntilMatchOrAccept(Thread(pc + 1, Set(pc) ++ progress, priority, parse), todo, result)
        }
        case Jump(offset) => {
          runUntilMatchOrAccept(Thread(pc + offset, progress, priority, parse), todo, result)
        }
        case Fork(offset1, offset2) => {
          val next_todo = Set(Thread(pc + offset2, progress, priority + 'r', parse)) ++ todo
          runUntilMatchOrAccept(Thread(pc + offset1, progress, priority + 'l', parse), next_todo, result)
        }
        case `PushEmpty` => runUntilMatchOrAccept(Thread(pc + 1, progress, priority, EmptyLeaf +: parse), todo, result)
        case `PushChar` => {
          throw new Exception("runUntilMatchOrAccept should not have reached a PushChar instruction. This should be handled in matchStringPosition.")
        }

        case `PushLeft` => runUntilMatchOrAccept(Thread(pc + 1, progress, priority, LeftNode(parse.head) +: parse.tail), todo, result)
        case `PushRight` => runUntilMatchOrAccept(Thread(pc + 1, progress, priority, RightNode(parse.head) +: parse.tail), todo, result)
        case `PushConcat` => {
          val right = parse.head
          val left = parse.tail.head
          val rest = parse.tail.tail
          runUntilMatchOrAccept(Thread(pc + 1, progress, priority, ConcatNode(left, right) +: rest), todo, result)
        }
        case `InitStar` => runUntilMatchOrAccept(Thread(pc + 1, progress, priority, StarNode(Seq()) +: parse), todo, result)
        case `PushStar` => {
          val body = parse.head
          val star = parse.tail.head
          val rest = parse.tail.tail
          star match {
            case StarNode(seq) =>
              runUntilMatchOrAccept(Thread(pc + 1, progress, priority, StarNode(body +: seq) +: rest), todo, result)
            case _ =>
              throw new Exception("should be unreachable")
          }
        }
        case PushCapture(name) => runUntilMatchOrAccept(Thread(pc + 1, progress, priority, CaptureNode(name, parse.head) +: parse.tail), todo, result)
      }
    }

    // Remove any threads s.t. there exists another thread at the same program
    // point with a smaller Priority.
    def compact(threads: Set[Thread]): Set[Thread] = threads.groupBy(_.pc).map { case (pc, t_group) => t_group.minBy(_.priority) }.toSet

    // Return the result of matching the current string position on all the
    // given threads.
    val matchStringPosition: (Set[Thread], Char) => Set[Thread] = (threads, c) => {
      threads.filter(t => program(t.pc) match {
        case `Accept` => false
        case MatchSet(charset) => if (charset.contains(c)) true else false
        case _ => throw new Exception("Encountered unexpected instruction in matchStringPosition")
      }).map(t => program(t.pc + 1) match {
        case `PushChar` => Thread(t.pc + 2, Set(), t.priority, CharLeaf(c) +: t.parse)
        case _ => Thread(t.pc + 1, Set(), t.priority, t.parse)
      })
    }

    def run(sp: Int, threads: Set[Thread]): Option[ParseTree] = {
      if (threads.isEmpty) return None
      val pending_threads = compact(runUntilMatchOrAccept(threads.head, threads.tail, Set()))
      if (sp == str.length) {
        val accepted_threads = pending_threads.filter(t => program(t.pc) == `Accept`)
        if (accepted_threads.isEmpty) None else Some(accepted_threads.head.parse.head)
      } else {
        run(sp + 1, matchStringPosition(pending_threads, str(sp)))
      }
    }

    run(0, Set(Thread(0, Set(), "", Seq())))
  }

  // A thread of execution for the VM, where 'pc' is the program counter,
  // 'progress' is the set of encountered CheckProgress instructions, 'priority'
  // is the thread priority (lower is better), 'parse' is the current parsing
  // stack. We don't need a separate string position per thread because all
  // executing threads will, by construction, always be at the same string
  // position.
  private case class Thread(pc: Int, progress: Set[Int], priority: String,
    parse: Seq[ParseTree])
}
