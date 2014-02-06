package info.drealm.s3pi

trait Sims3ResourceHandler {

    /**
     * The set of resource types supported by this resource handler
     */
    def supportedResourceTypes: Seq[ResourceType]

    /**
     * Calls the appropriate constructor to extract the identified resource
     * and instantiate it
     */
    def apply(pkg: Sims3Package, resourceIdentifier: ResourceIdentifier): Sims3Resource

    /**
     * Creates a new instance of the resource type
     */
    def apply(resourceType: ResourceType): Sims3Resource

    protected def internalApply(resourceType: ResourceType, f: Unit => Sims3Resource): Sims3Resource = {
        resourceType match {
            case x if (supportedResourceTypes.contains(x)) => f()
            case _ => throw new Exception("Unsupported resource type")
        }
    }

}
