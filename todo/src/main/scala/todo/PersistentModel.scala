package todo

import cats.implicits.*
import java.nio.file.{Path, Paths, Files}
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption
import io.circe.*
import java.io.*
import io.circe.parser.*
import io.circe.syntax.*
import scala.collection.mutable
import todo.data.*

/**
 * The PersistentModel is a model that saves all data to file, meaning that
 * tasks persist between restarts.
 *
 * You should modify this file.
 */
object PersistentModel extends Model:
  import Codecs.given

  /**
   * Load Tasks from a file. Throws an exception on failure.
   */
  def loadTasks(path: Path): Tasks =
    load[Tasks](path)

  /**
   * Load an Id from a file. Throws an exception on failure.
   */
  def loadId(path: Path): Id =
    load[Id](path)

  /**
   * Load JSON-encoded data from a file.
   *
   * Given a file name, load JSON data from that file, and decode it into the
   * type A. Throws an exception on failure.
   *
   * It is not necessary to use this method. You should be able to use loadTasks
   * and loadId instead, which have a simpler interface.
   */
  def load[A](path: Path)(using decoder: Decoder[A]): A = {
    val str = Files.readString(path, StandardCharsets.UTF_8)
    // In a production system we would want to pay more attention to error
    // handling than we do here, but this is sufficient for the case study.
    decode[A](str) match {
      case Right(result) => result
      case Left(error) => 
        throw error
    }
  }

  /**
   * Save tasks to a file. If the file already exists it is overwritten.
   */
  def saveTasks(path: Path, tasks: Tasks): Unit =
    save(path, tasks)

  /**
   * Save Id to a file. If the file already exists it is overwritten.
   */
  def saveId(path: Path, id: Id): Unit =
    save(path, id)

  /**
   * Save data to a file in JSON format.
   *
   * Given a file name and some data, saves that data to the file in JSON
   * format. If the file already exists it is overwritten.
   *
   * It is not necessary to use this method. You should be able to use saveTasks
   * and saveId instead, which have a simpler interface.
   */
  def save[A](path: Path, data: A)(using encoder: Encoder[A]): Unit =
    val json = data.asJson
    Files.writeString(path, json.spaces2, StandardCharsets.UTF_8)
    ()

  /* Hint: there are two pieces of state we need to implement the model:
   * - the tasks
   * - the next Id
   * (The InMemoryModel uses the same.)
   */

  // val idFilePath = Path.of("/tmp/loadId.json")
  // val tasksFilePath = Path.of("/tmp/loadTasks.json")

  def createFilesIfNotExistWithDefaultState: (Path, Path) = 
    val idFile = new File("identifier.json")
    val tasksFile = new File("tasks.json")
    if !idFile.exists() then
      idFile.createNewFile()
      saveId(idFile.toPath, Id(0))
    end if
    if !tasksFile.exists() then
      tasksFile.createNewFile()
    end if
    (idFile.toPath, tasksFile.toPath)

  def createFile(fileName: String): Path =
    val f = new File(fileName)
    f.createNewFile()
    f.toPath
    

  val (idFilePath, tasksFilePath)  = createFilesIfNotExistWithDefaultState

  def create(task: Task): Id =
    val id = loadId(idFilePath)
    val tasks = loadTasks(tasksFilePath)
    saveTasks(tasksFilePath, tasks.copy(((Id(id.toInt + 1), task) :: tasks.tasks.toList).sortBy(e => e._1.toInt)))
    saveId(idFilePath, Id(id.toInt + 1))
    Id(id.toInt + 1)

  def read(id: Id): Option[Task] =
    val tasks = loadTasks(tasksFilePath)
    tasks.tasks.find(e => e._1 == id).map(e => e._2)

  def update(id: Id)(f: Task => Task): Option[Task] =
    val tasks = loadTasks(tasksFilePath)
    val res = tasks.tasks.find(e => e._1 == id).map(e => (e._1, f(e._2)))
    res.foreach(e => saveTasks(tasksFilePath, 
    Tasks(e :: tasks.tasks.filter(e => e._1 != id).toList.sortBy(e => e._1.toInt))))
    res.map(_._2)

  def delete(id: Id): Boolean =
    val tasks = loadTasks(tasksFilePath)
    val res = tasks.tasks.filter(e => e._1 != id).toList
    if res.length == tasks.tasks.toList.length then false
    else 
      saveTasks(tasksFilePath, Tasks(res.sortBy(e => e._1.toInt)))
      true

  def tasks: Tasks =
    loadTasks(tasksFilePath)

  def tasks(tag: Tag): Tasks =
    Tasks(loadTasks(tasksFilePath).tasks
    .filter(e => e._2.tags.contains(tag)))

  def complete(id: Id): Option[Task] =
    val tasks = loadTasks(tasksFilePath).tasks
    val m = tasks.find(e => e._1 == id).map(e => e._2)
    m match
      case Some(i) => 
        val filteredTasks = tasks.filter(e => e._1 != id).toList
        val updatedState = (id, i.copy(state = State.completedNow))
        saveTasks(tasksFilePath, Tasks((updatedState :: filteredTasks).sortBy(e => e._1.toInt)))
        Some(updatedState._2)
      case _ => None

  def tags: Tags =
    val tasks = loadTasks(tasksFilePath).tasks
    Tags(tasks.toList.flatMap(e => e._2.tags.toSet).toSet.toList.sortBy(e => e.tag))

  def clear(): Unit =
    saveTasks(tasksFilePath, Tasks(List.empty))
