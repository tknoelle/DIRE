package core.config

/**
 * User: nowi
 * Date: 11.03.2010
 * Time: 11:26:08
 */
import containers.heuristics.{LightestClauseHeuristicStorage, ListBufferStorage}
import resolution.{PositiveFactorer, DALCUniqueLiteralResolver, DALCResolver}
import containers.{MutableClauseStore, STIndex, CNFClauseStore}
import core._
import core.reduction._
import core.rewriting.{VariableRewriter}
import ordering.{CustomConferencePartitionedPrecedence, ALCLPOComparator}
import org.neo4j.kernel.EmbeddedGraphDatabase
import recording.{NaiveClauseRecorder, Neo4JRecorder}
import selection.{DALCRSelector}

object DALCConfig {
  // the initial clause store

  lazy val variableRewriter = new VariableRewriter
  lazy val standardizer = new Standardizer(this)

  lazy val neo4JGraphBasePath: String = "/workspace/DIRE/DIRE/logs/graph/clauses"

  // unique literal resolver
  lazy val uniqueLiteralResolver = new DALCUniqueLiteralResolver(this)

  // ordered resolution needs comparator and selection
  lazy val precedence = new CustomConferencePartitionedPrecedence
  lazy val literalComparator = new ALCLPOComparator(this)
  lazy val selector = new DALCRSelector()

  // forward subsumer WITH index support
  lazy val forwardSubsumer = ForwardSubsumer


  // positive factorer
  lazy val positiveFactorer = PositiveFactorer

  // ACL resolver
  lazy val resolver = new DALCResolver(this)
  lazy val subsumptionStrategy = StillmannSubsumer
  lazy val inferenceRecorder = new NaiveClauseRecorder


  // usable clause store with STI indexes
  def usableClauseStore = new MutableClauseStore with LightestClauseHeuristicStorage with STIndex

  def workedOffClauseStore = new MutableClauseStore with ListBufferStorage with STIndex

  // switches
  // TODO enable all reductions

  val recordProofSteps = true


  // set a time limit
  val timeLimit: Long = (10 * 60 * 1000) // 10 min
}