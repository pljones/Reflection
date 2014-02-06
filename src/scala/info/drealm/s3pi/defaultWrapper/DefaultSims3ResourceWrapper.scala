package info.drealm.s3pi.defaultWrapper

import info.drealm.s3pi._

object DefaultSims3ResourceHandler extends Sims3ResourceHandler {
    override def supportedResourceTypes: Seq[ResourceType] = Nil
    override def apply(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier) = new DefaultSims3Resource(pkg, resourceIdentifier)
    override def apply(resourceType: ResourceType) = new DefaultSims3Resource
}

class DefaultSims3Resource extends Sims3Resource {
    def this(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier) = { this(); load(pkg, resourceIdentifier) }
    def load(byteStream: io.Source) = new Exception("Unsupported operation")
}
