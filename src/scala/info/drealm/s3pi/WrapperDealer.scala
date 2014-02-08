package info.drealm.s3pi

import scala.reflect.runtime.{ universe => ru }

object WrapperDealer {

    def createNewResource(resourceType: ResourceType): Sims3Resource = {
        getSims3ResourceHandlerFor(resourceType)(resourceType)
    }

    def getResource(pkg: Sims3Package)(resourceIdentifier: ResourceIdentifier): Sims3Resource = {
        getResource(pkg, resourceIdentifier, false)
    }
    def getBareResource(pkg: Sims3Package)(resourceIdentifier: ResourceIdentifier): Sims3Resource = {
        getResource(pkg, resourceIdentifier, true)
    }

    // === I == M == P == L == E == M == N == T == A == T == I == O == N ===

    private[this] class WrapperId(
        val resourceType: ResourceType,
        val sims3ResourceHandler: Sims3ResourceHandler)

    private[this] def getSims3ResourceHandlerFor(resourceType: ResourceType): Sims3ResourceHandler = {
        wrappers.find(w => w.resourceType == resourceType) match {
            case Some(x) => x.sims3ResourceHandler
            case None => info.drealm.s3pi.defaultWrapper.DefaultSims3ResourceHandler
        }
    }

    private[this] def getResource(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier, isBare: Boolean): Sims3Resource = {
        val h =
            if (isBare)
                info.drealm.s3pi.defaultWrapper.DefaultSims3ResourceHandler
            else
                getSims3ResourceHandlerFor(resourceIdentifier.resourceType)
        h(pkg, resourceIdentifier)
    }

    // === C == O == N == S == T == R == U == C == T == O == R ===

    private val (places, names) = getClassPaths("Wrappers", "bin") // TODO: Configurable runtime class path

    // Now we need to tell the runtime where to look
    private val cl = new java.net.URLClassLoader(places)

    // And get a runtime mirror to look up symbols
    private val rm = ru.runtimeMirror(cl)

    // Look up every name we found (ignoring anything that won't load)
    private val allClasses = List((for (
        name <- names;
        opt = try { Some(rm.classSymbol(cl.loadClass(name))) } catch { case ex: Throwable => None };
        if (opt match { case Some(_) => true; case _ => false })
    ) yield opt.get): _*)

    // This is the symbol we are looking for
    private val sims3ResourceHandler = ru.typeOf[Sims3ResourceHandler].typeSymbol

    // Look for needles in the haystack
    private val handlers = for (
        clazz <- allClasses;
        if !clazz.isTrait && clazz.typeSignature.baseClasses.contains(sims3ResourceHandler)
    ) yield {
        // The next line does magic I do not comprehend - and I forget where I read it...
        val classSymbol = clazz.owner.typeSignature.member(clazz.name.toTermName)
        val mm = rm.reflectModule(classSymbol.asModule)
        mm.instance.asInstanceOf[Sims3ResourceHandler]
    }

    // Finally, set up the wrapper registry
    private val wrappers = for (handler <- handlers; resType <- handler.supportedResourceTypes)
        yield new WrapperId(resType, handler)

    // === I == M == P == L == E == M == N == T == A == T == I == O == N ===

    private[this] def getClassPaths(paths: String*): Tuple2[Array[java.net.URL], Seq[String]] = {
        def recursiveListFiles(path: java.io.File): Array[java.io.File] = {
            val (files, directories) = path.listFiles partition (!_.isDirectory)
            files ++ directories.flatMap(recursiveListFiles)
        }
        def fromJarPath(path: java.io.File): Array[String] = {
            import collection.JavaConverters._
            (for (
                entry <- (new java.util.zip.ZipFile(path)).entries.asScala
            ) yield entry.getName).toArray
        }

        // Get all the interesting files in each path
        val (classFiles, jarFiles) = List((for (
            path <- paths;
            file <- recursiveListFiles(new java.io.File(path)).toList;
            lname = file.getName.toLowerCase;
            if (lname.endsWith(".class") || lname.endsWith(".jar"))
        ) yield (lname.endsWith(".class"), path, file.getCanonicalFile)): _*) partition (x => x._1)

        // Places represents the new classpath locations -- should match the "paths" passed in but cleaned up
        val places = Set((for (file <- classFiles ::: jarFiles) yield new java.io.File(file._2)).map(_.toURI.toURL): _*).toSeq
        // Names represents all the classes
        val names = Seq[String]()

        val classNames = (for (
            t <- classFiles;
            path = new java.io.File(t._2).getCanonicalPath;
            afterPath = path.length + 1;
            file = t._3
        ) yield {
            file.getCanonicalPath().substring(afterPath).replace('/', '.').replace('\\', '.')
        }).map(x => x.substring(0, x.length() - 6))

        val jarClasses = (for (
            t <- jarFiles;
            jar = t._3;
            resource <- fromJarPath(jar) if resource.toLowerCase.endsWith(".class")
        ) yield resource.replace('/', '.').replace('\\', '.'))
            .map(x => x.substring(0, x.length() - 6)) // no idea if front needs trimming

        (places.toArray, classNames ::: jarClasses)
    }

}