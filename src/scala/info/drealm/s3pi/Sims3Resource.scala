package info.drealm.s3pi

trait Sims3Resource {

    def load(byteStream: io.Source): Unit

    protected def getStream(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier): Option[io.Source] = {
        pkg.find(resourceIdentifier)
    }

    protected def load(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier): Unit =
        getStream(pkg, resourceIdentifier) match {
            case Some(stream) => load(stream)
            case None => throw new Exception("Resource not found")
        }
}
