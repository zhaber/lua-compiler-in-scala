package ca.mcmaster.lasci.middle_end
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import ca.mcmaster.lasci.front_end.Statement
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap
import org.slem.IRTree.L_Value
import scala.collection.mutable.ListBuffer

class SymbolTable {

  private val table = new HashMap[String, collection.mutable.Set[Symbol]] with MultiMap[String, Symbol]

  override def toString() = {
    ("" /: table)((entry1, entry2) => entry1 + "\n" + (("" /: entry2._2)((type1, type2) => type1 + ",\n" + type2).tail.tail))
  }
  
  def symbols() = (ListBuffer[Symbol]() /: table.values) (_ ++ _)

  def put(symbol: Symbol): Unit = {
    val tableSymbols = table.get(symbol.name)
    tableSymbols match {
      case Some(tableSymbols) => {
        for (tableSymbol <- tableSymbols) {
          if (symbol == tableSymbol) {
        	val newSymbol =	new Symbol(symbol.name, symbol.ltypes, symbol.scope)
        	table.removeBinding(symbol.name, tableSymbol)
            table.addBinding(symbol.name, newSymbol)
            return
          }
        }
        table.addBinding(symbol.name, symbol)
      }
      case None => table.addBinding(symbol.name, symbol)
    }
  }

  private def getDepth(parentBlocks: List[List[Statement]], block: List[Statement]): Option[Int] = {
    val depth = 0;
    for (parentBlock <- parentBlocks) {
      if (parentBlock == block) {
        return Some(depth)
      }
    }
    None
  }

  private def getNearer(parentBlocks: List[List[Statement]], nearestSymbol: Option[Symbol], symbol: Symbol): Option[Symbol] = {
    symbol.scope match {
      case Global => {
        if (nearestSymbol.isEmpty) {
          Some(symbol)
        } else {
          nearestSymbol
        }
      }
      case Local(statements) => {
        val depth = getDepth(parentBlocks, statements)
        if (depth.isDefined) {
          nearestSymbol match {
            case None => {
              Some(symbol)
            }
            case Some(nearestSymbol) => {
              nearestSymbol.scope match {
                case Global => Some(symbol)
                case Local(nearestStatements) => {
                  if (depth.get < getDepth(parentBlocks, nearestStatements).get) {
                    Some(symbol)
                  } else {
                    Some(nearestSymbol)
                  }
                }
              }
            }
          }
        } else {
          nearestSymbol
        }
      }
    }
  }

  def get(name: String, parentBlocks: List[List[Statement]]): Option[Symbol] = {
    val symbols = table.get(name)
    symbols match {
      case Some(symbols) => {
        var nearestSymbol: Option[Symbol] = None
        for (symbol <- symbols) {
          nearestSymbol = getNearer(parentBlocks, nearestSymbol, symbol)
        }
        nearestSymbol
      }
      case None => None
    }
  }
  
}