//  a "Compiler" for the Brainf*** language

object M5b {

// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}

type Mem = Map[Int, Int]

import io.Source
import scala.util._

// (1)
def load_bff(name: String) : String = {
    // try Source.fromFile(name).mkString
    // catch {
    //     case _: Exception => ""
    // }
    Try(Source.fromFile(name).toList).getOrElse(Nil).mkString
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem+(mp->v)
}

// (3) 

def jumpRight(prog: String, pc: Int, level: Int) : Int = pc match {
    case x if x < prog.length => prog.charAt(pc) match {
        case '[' => jumpRight(prog, pc+1, level+1)
        case ']' if (level==0) => pc+1
        case ']' if (level>0) => jumpRight(prog, pc+1, level-1)
        case _ => jumpRight(prog, pc+1, level)
    }
    case _ => pc
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = pc match{
    case i if i >= 0 => prog.charAt(pc) match {
        case ']' => jumpLeft(prog, pc-1, level+1)
        case '[' if (level==0) => pc+1
        case '[' if (level>0) => jumpLeft(prog, pc-1, level-1)
        case _ => jumpLeft(prog, pc - 1, level)
    }
    case _ => pc
}


// testcases
// jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
// jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
// jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
// jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
// jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
// jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)

// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc<prog.length && pc >= 0){ 
        prog(pc) match {
            case '>' => compute(prog,pc+1,mp+1,mem)
            case '<' => compute(prog,pc+1,mp-1,mem)
            case '+' => compute(prog,pc+1,mp,write(mem,mp,sread(mem,mp)+1))
            case '-' => compute(prog,pc+1,mp,write(mem,mp,sread(mem,mp)-1))
            case '.' => print(sread(mem,mp).toChar)
                        compute(prog,pc+1,mp,mem)
            case '[' if sread(mem,mp)==0 =>compute(prog, jumpRight(prog,pc+1,0),mp,mem)
            case ']' if sread(mem,mp)!=0 =>compute(prog, jumpLeft(prog,pc-1,0),mp,mem)
            case _ => compute(prog,pc+1,mp,mem)
        }
    } else mem
}

def run(prog: String, m: Mem = Map()) = {
    compute(prog,0,0,m)
}

// (5)
def generate(msg: List[Char]) : String = {
    val ascil = for (x<-msg) yield ("+"*x.toInt)+".[-]"
    ascil.mkString
}

// generate("ABC".toList)

// (6) 
def jtable(pg: String) : Map[Int, Int] = {
  val pgl = (0 until pg.length).toList
  pgl.filter(x=>pg(x)=='[').map(y=>y->jumpRight(pg,y+1,0)).flatMap(z=>List(z,(z._2-1)->(z._1+1))).toMap
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<pg.length && pc >= 0){ 
        pg(pc) match {
            case '>' => compute2(pg,tb,pc+1,mp+1,mem)
            case '<' => compute2(pg,tb,pc+1,mp-1,mem)
            case '+' => compute2(pg,tb,pc+1,mp,write(mem,mp,sread(mem,mp)+1))
            case '-' => compute2(pg,tb,pc+1,mp,write(mem,mp,sread(mem,mp)-1))
            case '.' => print(sread(mem,mp).toChar)
                        compute2(pg,tb,pc+1,mp,mem)
            case '[' if sread(mem,mp)==0 =>compute2(pg,tb,tb(pc),mp,mem)
            case ']' if sread(mem,mp)!=0 =>compute2(pg,tb,tb(pc),mp,mem)
            case _ => compute2(pg,tb,pc+1,mp,mem)
        }
    } else mem
}
def run2(pg: String, m: Mem = Map()) = {
  compute2(pg,jtable(pg),0,0,m)
}

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

def optimise(s: String) : String = {
  val exp1 = """[^<>+\-.\[\]]"""
  val exp2 = """\[-\]"""
  s.replaceAll(exp1,"").replaceAll(exp2,"0")
  //replace all occurrences of exp1 with "" and exp2 with zeroes
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<pg.length && pc >= 0){ 
        pg(pc) match {
            case '>' => compute3(pg,tb,pc+1,mp+1,mem)
            case '<' => compute3(pg,tb,pc+1,mp-1,mem)
            case '+' => compute3(pg,tb,pc+1,mp,write(mem,mp,sread(mem,mp)+1))
            case '-' => compute3(pg,tb,pc+1,mp,write(mem,mp,sread(mem,mp)-1))
            case '.' => print(sread(mem,mp).toChar)
                        compute3(pg,tb,pc+1,mp,mem)
            case '[' if sread(mem,mp)==0 =>compute3(pg,tb,tb(pc),mp,mem)
            case ']' if sread(mem,mp)!=0 =>compute3(pg,tb,tb(pc),mp,mem)
            case '0' => compute3(pg,tb,pc+1,mp,write(mem,mp,0))//command zero writes to mem
            case _ => compute3(pg,tb,pc+1,mp,mem)
        }
    } else mem
}

def run3(pg: String, m: Mem = Map()) = {
  val optig = optimise(pg)
  compute3(optig,jtable(optig),0,0,m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203

// time_needed(1, run3(load_bff("benchmark.bf")))

// (8) 

//helper functions to make determining valid symbols easier
val ops = List("+", "-", "<", ">")
def is_op(op: String) : Boolean = ops.contains(op)

def combine(s: String, acc:Int = 1, out:String = "") : String = {
  val sl = s.sliding(1).toList
  sl match {
    case c::rest if is_op(c) => c::rest match {
      case c::rest if(c == rest.head) => acc match{
        case 26 => combine(rest.mkString, 1, out+c+(acc+64).toChar.toString)
        case _ => combine(rest.mkString, acc+1, out)
      }
      case c::rest => combine(rest.mkString, 1, out+c+(acc+64).toChar.toString)
      }
    case c::rest if !(is_op(c)) => combine(rest.mkString, 1, out+c)
    case _ => out
  }
}
// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc<pg.length && pc >= 0){ 
        pg(pc) match {
            case '>' => compute4(pg,tb,pc+2,mp+(pg.charAt(pc+1).toInt-64),mem)
            case '<' => compute4(pg,tb,pc+2,mp-(pg.charAt(pc+1).toInt-64),mem)
            case '+' => compute4(pg,tb,pc+2,mp,write(mem,mp,sread(mem,mp)+(pg.charAt(pc+1).toInt-64)))
            case '-' => compute4(pg,tb,pc+2,mp,write(mem,mp,sread(mem,mp)-(pg.charAt(pc+1).toInt-64)))
            case '.' => print(sread(mem,mp).toChar)
                        compute4(pg,tb,pc+1,mp,mem)
            case '[' if sread(mem,mp)==0 =>compute4(pg,tb,tb(pc),mp,mem)
            case ']' if sread(mem,mp)!=0 =>compute4(pg,tb,tb(pc),mp,mem)
            case '0' => compute4(pg,tb,pc+1,mp,write(mem,mp,0))//command zero writes to mem
            case _ => compute4(pg,tb,pc+1,mp,mem)
        }
    } else mem
}

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
  val optigbine = combine(optimise(pg))
  compute4(optigbine,jtable(optigbine),0,0,m)
}


// // testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// // testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}


