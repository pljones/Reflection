package info.drealm.s3pi.exampleWrapper

import info.drealm.s3pi._

object ExampleResourceHandler extends Sims3ResourceHandler {

    override def supportedResourceTypes = Seq(new ResourceType(24))

    override def apply(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier) =
        internalApply(resourceIdentifier.resourceType, _ => { new ExampleResource(pkg, resourceIdentifier) })

    override def apply(resourceType: ResourceType) = internalApply(resourceType, _ => { new ExampleResource })

}

class ExampleResource extends Sims3Resource {
    def this(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier) = { this(); load(pkg, resourceIdentifier) }
    def load(byteStream: io.Source) = new Exception("Unsupported operation")
}