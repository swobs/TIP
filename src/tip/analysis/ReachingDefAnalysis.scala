package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.cfg.CfgOps._
import tip.cfg._
import tip.lattices._
import tip.solvers._

/**
  * Base class for reaching def analysis.
  */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  // TW: lattice has to be built from the powerset of all assignment statements in the program
  val allAssignments: Set[AAssignStmt] = cfg.nodes.flatMap(_.appearingAssignments)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allAssignments))

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
    // TW: reaching definitions for program point p are those assignments that may have defined the current values of
    // variables
    // lattice is powerset lattice of all assignments in program
    // for every cfg node v constraint variable [[v]] denotes set of assignments that may define values of variables
    // at program point AFTER v
    // transfer rules: SPA, p. 73f
    // similar to localTransfer() in SimpleSignAnalysis
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              // TW: union of set difference s \ old assignment of id and as (actual assignment for id)
                // HP: you can use filtet to just keep from s the ones one the appearingIds are not a super set (is gonna be a singleton, but the method returns a set) of the appearingIds of id
              case id: AIdentifier => s - ... + as ??? //<--- Complete here
              // TW: other nodes: [[v]] = join(v) = s
              case _ => s
            }
          case _ => s
        }
      case _ => s
    }
  }
}

/**
  * Reaching def analysis that uses the simple fixpoint solver.
  */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies // TW: changed

/**
  * Reaching def analysis that uses the worklist solver.
  */
class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies // TW: changed
