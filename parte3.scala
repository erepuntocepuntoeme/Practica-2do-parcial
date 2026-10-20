// EJERCICIO 1
abstract class RecursoBiblioteca(
  val titulo: String,
  val autor: String,
  val anio: Int
) {
  def tipoRecurso: String

  def descripcion: String =
    s"$titulo - $autor ($anio) - $tipoRecurso"

  def costoPrestamo(dias: Int): Double
}

// EJERCICIO 2
trait Prestable {
  val disponible: Boolean
  def prestar(): RecursoBiblioteca
}

// EJERCICIO 3
trait Renovable {
  val diasPrestamo: Int
  def renovar(diasExtra: Int): RecursoBiblioteca
}

// EJERCICIO 4
trait Descargable {
  val tamanioMB: Double
  def descargar(): String =
    s"Descargando archivo de $tamanioMB MB"
}

// EJERCICIO 5
trait Reproducible {
  val duracionMinutos: Int
  def reproducir(): String =
    s"Reproduciendo contenido de $duracionMinutos minutos"
}

// EJERCICIO 6
class LibroFisico(
  titulo: String,
  autor: String,
  anio: Int,
  val disponible: Boolean,
  val diasPrestamo: Int
) extends RecursoBiblioteca(
      titulo,
      autor,
      anio
    )
    with Prestable
    with Renovable {

  override def tipoRecurso: String =
    "Libro Físico"

  override def costoPrestamo(dias: Int): Double =
    dias * 1.5

  override def prestar(): LibroFisico =
    new LibroFisico(
      titulo,
      autor,
      anio,
      false,
      diasPrestamo
    )

  override def renovar(diasExtra: Int): LibroFisico =
    new LibroFisico(
      titulo,
      autor,
      anio,
      disponible,
      diasPrestamo + diasExtra
    )
}

// EJERCICIO 7
class LibroDigital(
  titulo: String,
  autor: String,
  anio: Int,
  val tamanioMB: Double
) extends RecursoBiblioteca(
      titulo,
      autor,
      anio
    )
    with Descargable {

  override def tipoRecurso: String =
    "Libro Digital"

  override def costoPrestamo(dias: Int): Double =
    dias * 0.5
}

// EJERCICIO 8
class Audiolibro(
  titulo: String,
  autor: String,
  anio: Int,
  val duracionMinutos: Int,
  val tamanioMB: Double
) extends RecursoBiblioteca(
      titulo,
      autor,
      anio
    )
    with Reproducible
    with Descargable {

  override def tipoRecurso: String =
    "Audiolibro"

  override def costoPrestamo(dias: Int): Double =
    dias * 0.75
}

// EJERCICIO 9
class RevistaFisica(
  titulo: String,
  autor: String,
  anio: Int,
  val numeroEdicion: Int,
  val disponible: Boolean,
  val diasPrestamo: Int
) extends RecursoBiblioteca(
      titulo,
      autor,
      anio
    )
    with Prestable
    with Renovable {

  override def tipoRecurso: String =
    "Revista Física"

  override def costoPrestamo(dias: Int): Double =
    dias * 1.0

  override def prestar(): RevistaFisica =
    new RevistaFisica(
      titulo,
      autor,
      anio,
      numeroEdicion,
      false,
      diasPrestamo
    )

  override def renovar(diasExtra: Int): RevistaFisica =
    new RevistaFisica(
      titulo,
      autor,
      anio,
      numeroEdicion,
      disponible,
      diasPrestamo + diasExtra
    )
}

// EJERCICIO 10
class VideoCurso(
  titulo: String,
  autor: String,
  anio: Int,
  val duracionMinutos: Int,
  val tamanioMB: Double
) extends RecursoBiblioteca(
      titulo,
      autor,
      anio
    )
    with Reproducible
    with Descargable {

  override def tipoRecurso: String =
    "Video Curso"

  override def costoPrestamo(dias: Int): Double =
    dias * 1.2
}

// EJERCICIO 11
def mostrarInformacion(recurso: RecursoBiblioteca): String =
  recurso.descripcion

// EJERCICIO 12
def calcularCosto(recurso: RecursoBiblioteca, dias: Int): Double =
  recurso.costoPrestamo(dias)

object Parte3 {
  def main(args: Array[String]): Unit = {
    // EJERCICIO 13
    val libro1 = new LibroFisico("Clean Code", "Robert C. Martin", 2008, true, 7)
    val libro2 = new LibroDigital("Functional Programming in Scala", "Paul Chiusano", 2023, 18.5)
    val audio1 = new Audiolibro("The Pragmatic Programmer", "Andrew Hunt", 1999, 720, 850.0)
    val revista1 = new RevistaFisica("IEEE Software", "IEEE", 2026, 8, true, 5)
    val curso1 = new VideoCurso("Scala Avanzado", "Academia FP", 2026, 240, 1500.0)

    // EJERCICIO 14
    println(mostrarInformacion(libro1))
    println(mostrarInformacion(libro2))
    println(mostrarInformacion(audio1))
    println(calcularCosto(libro1, 10))
    println(calcularCosto(curso1, 5))

    // EJERCICIO 15
    println(libro2.descargar())
    println(audio1.descargar())
    println(audio1.reproducir())
    println(curso1.reproducir())

    // EJERCICIO 16
    val libroPrestado = libro1.prestar()
    println(libro1.disponible)
    println(libroPrestado.disponible)

    // EJERCICIO 17
    val libroRenovado = libro1.renovar(5)
    println(libro1.diasPrestamo)
    println(libroRenovado.diasPrestamo)
  }
}
