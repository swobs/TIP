package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData

import tip.solvers._
import tip.cfg._

/**
  * Base class for live variables analysis.
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  val allVars: Set[ADeclaration] = cfg.nodes.flatMap(_.appearingIds)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars))

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
    // TW: variable is live at program point p if there exists an execution where its value is read after program point
    // and variable is not being written to between program point p and being read
    // for every cfg node n constraint variable [[v]] is subset of variables that are live BEFORE n
    // (subset may be too large but never too small = sound)
    // join function joins with constraints of SUCCESSORS of v
    // why?
    // if variable x is live at v_2 then it has to be live at v_1, too if v_1 is not an assignment to x
    // transfer rules: SPA, p. 63f
    // similar to localTransfer() in SimpleSignAnalysis
    n match {
      // TW: don't know why there is a type error
      // maybe some dependency on the last exercise; I just pulled your last feedback...
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          // TW: union of s with vars appearing in expression
          case cond: AExpr => s + set of vars ??? //<--- Complete here
          case as: AAssignStmt =>
            as.left match {
              // TW: union of set difference s \ id with vars appearing in expression on right hand side
              case id: AIdentifier => s - id + set of vars ??? //<--- Complete here
              // TW: other nodes: [[v]] = join(v) = s
              case _ => s
            }
          // TW: [[v]] = join(v) \ set of vars
          case varr: AVarStmt => s + set of vars ??? //<--- Complete here
          // TW: union of s with vars appearing in expression; I think that was the missing rule in SPA lecture notes
          case ret: AReturnStmt => s + set of vars ??? //<--- Complete here
          // TW: union of s with vars appearing in expression
          case out: AOutputStmt => s + set of vars ??? //<--- Complete here
          case _ => s
        }
      case _ => s
    }
  }
}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
