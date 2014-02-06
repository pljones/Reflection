package info.drealm.s3pi

import scala.tools.nsc.util.ScalaClassLoader
import scala.reflect.runtime.{ universe => ru }
import info.drealm.s3pi._

object WrapperDealer {
    private[this] class WrapperId(
        val resourceType: ResourceType,
        val sims3ResourceHandler: Sims3ResourceHandler)

    //TODO: Load list of file names to skip
    //TODO: Allow class path to be configured
    //TODO: Load list of class names to skip

    private[this] def recursiveListFiles(path: java.io.File): Array[java.io.File] = {
        val (files, directories) = path.listFiles partition (!_.isDirectory)
        files ++ directories.flatMap(recursiveListFiles)
    }

    private[this] def fromJarPath(path: java.io.File): Array[String] = {
        import collection.JavaConverters._
        (for (
            entry <- (new java.util.zip.ZipFile(path)).entries.asScala
        ) yield entry.getName).toArray
    }

    val root = new java.io.File("bin")
    val rootPath = root.getCanonicalPath()
    val files = recursiveListFiles(root)

    val classFiles = files
        .filter(_.getName.toLowerCase.endsWith(".class"))

    val jarFiles = files
        .filter(_.getName.toLowerCase.endsWith(".jar"))

    // ===============================================================
    //
    // It appears that JVM supports no way of querying a jar or class
    // to determine its content, unlike .NET
    // So unless you already know the complete list of classes,
    // you cannot load them at runtime (thus making runtime loading
    // fairly pointless, as you know the complete list of classes
    // at compile time).
    // This hackiness works around the problem by searching out
    // classes by filename...
    //
    // ===============================================================

    // Scala will not tell us what classes exist in a classpath, so we have to go looking
    // Class files are easy...ish.
    val rootlen = root.getCanonicalPath().length + 1
    val classNames = classFiles
        .map(_.getCanonicalPath())
        .map(_.substring(rootlen))
        .map(x => x.substring(0, x.length() - 6))
        .map(_.replace('/', '.'))
        .map(_.replace('\\', '.'))

    // JAR files are a little trickier
    val jarClasses = (for (
        jar <- jarFiles;
        resource <- fromJarPath(jar) if resource.toLowerCase.endsWith(".class")
    ) yield resource)
        .map(x => x.substring(0, x.length() - 6)) // no idea if front needs trimming
        .map(_.replace('/', '.'))
        .map(_.replace('\\', '.'))

    // Right, now we need to tell Scala we've got a bunch of stuff:
    val cl = ScalaClassLoader
        .fromURLs(for (
            file <- classFiles ++ jarFiles
        ) yield file.toURI().toURL())

    // And get a runtime mirror to look up symbols
    val rm = ru.runtimeMirror(cl)

    // And there is still no nice way to say "here is a string, give me the type".
    // This assumes everything is a Class (it said ".class" on the end, after all)
    // and then filters out the failures.
    val allClasses = (for (
        name <- classNames ++ jarClasses;
        opt = try {
            Some(rm.classSymbol(Class.forName(name)))
        } catch {
            case ex: Throwable => None
        } if (opt match { case Some(_) => true; case _ => false })
    ) yield opt.get)

    // This is the symbol we are looking for
    val sims3ResourceHandler = ru.typeOf[Sims3ResourceHandler].typeSymbol

    // Filter out everything but what we want
    val wantedClasses = for (
        clazz <- allClasses;
        if !clazz.isTrait;
        if clazz.typeSignature.baseClasses.contains(sims3ResourceHandler)
    ) yield rm.reflectModule(clazz.owner.typeSignature.member(clazz.name.toTermName).asModule)

    println("")
    private val wrappers = List[WrapperId]()
    wantedClasses.foreach(mm => {
        val i = mm.instance.asInstanceOf[Sims3ResourceHandler]
        println(i)
    })

    /*
	 * http://stackoverflow.com/questions/8867766/scala-dynamic-object-class-loading
	 * This gets the *companion object* for the named class
	 */
    private[this] def companion[T](name: String)(implicit tag: reflect.ClassTag[T]): T =
        Class.forName(name + "$").getField("MODULE$").get(tag.runtimeClass).asInstanceOf[T]

    
    
    
    
    private[this] def getSims3ResourceHandlerFor(resourceType: ResourceType): Sims3ResourceHandler = {
        wrappers.find(w => w.resourceType == resourceType) match {
            case Some(x) => x.sims3ResourceHandler
            case None => info.drealm.s3pi.defaultWrapper.DefaultSims3ResourceHandler
        }
    }

    def createNewResource(resourceType: ResourceType): Sims3Resource = {
        getSims3ResourceHandlerFor(resourceType)(resourceType)
    }

    def getResource(pkg: Sims3Package)(resourceIdentifier: ResourceIdentifier): Sims3Resource = {
        getResource(pkg, resourceIdentifier, false)
    }
    def getBareResource(pkg: Sims3Package)(resourceIdentifier: ResourceIdentifier): Sims3Resource = {
        getResource(pkg, resourceIdentifier, true)
    }

    private def getResource(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier, isBare: Boolean): Sims3Resource = {
        (if (isBare) info.drealm.s3pi.defaultWrapper.DefaultSims3ResourceHandler
        else getSims3ResourceHandlerFor(resourceIdentifier.resourceType))(pkg, resourceIdentifier)
    }

}