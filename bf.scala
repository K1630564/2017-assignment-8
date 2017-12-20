// Part 2 about an Interpreter for the Brainf*** language
//========================================================

object CW8b {

  type Mem = Map[Int, Int]



  def sread(mem: Mem, mp: Int) : Int = {

    mem.getOrElse(mp, 0)

  }

  def write(mem: Mem, mp: Int, v: Int) : Mem = {

    mem.updated(mp, v)

  }


  def jumpRight(prog: String, pc: Int, level: Int) : Int = {

    if((pc > prog.length - 2) || (prog(pc) == ']' && level == 0) ) pc + 1

    else if (level != 0 && prog(pc) == ']') jumpRight(prog, pc + 1, level - 1)

    else if (prog(pc) == '[') jumpRight(prog, pc + 1, level + 1)

    else jumpRight(prog, pc + 1, level)


  }

  def jumpLeft(prog: String, pc: Int, level: Int) : Int = {

    if(level == 0 && prog(pc) == '[') pc + 1

    else if(level != 0 && pc == 0) - 1

    else if(level != 0 && prog(pc) == '[') jumpLeft(prog, pc - 1, level - 1)

    else if(prog(pc) == ']') jumpLeft(prog, pc - 1, level + 1)

    else jumpLeft(prog, pc - 1, level)


  }



  def run(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {

    if(pc > prog.length - 1) mem

    else {

      prog(pc) match {

        case '>' => run(prog, pc + 1, mp + 1, mem)

        case '<' => run(prog, pc + 1, mp - 1, mem)

        case '+' => run(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))

        case '-' => run(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))

        case '.' => print(sread(mem, mp).toChar)
          run(prog, pc + 1, mp, mem)

        case ',' => run(prog, pc + 1, mp, mem.updated(mp, Console.in.read().toByte))

        case '[' => {
          if(sread(mem, mp) == 0) run(prog, jumpRight(prog, pc + 1, 0), mp, mem)
          else run(prog, pc + 1, mp, mem) 
        }

        case ']' => {
          if(sread(mem, mp) != 0) run(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
          else run(prog, pc + 1, mp, mem)
        }

        case _ => run(prog, pc + 1, mp, mem)
      }
    }

  }

  def start(prog: String, mem: Mem) = {

    run(prog, 0, 0, mem)

  }

}
