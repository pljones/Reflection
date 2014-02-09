package info.drealm.s3pi

final class ResourceType(val resourceType: Int) extends Ordered[ResourceType] {
    override def equals(that: Any) =
        that.isInstanceOf[ResourceType] && compare(that.asInstanceOf[ResourceType]) == 0
    override def hashCode = resourceType
    override def compare(that: ResourceType) = this.resourceType.compare(that.resourceType)
    
    override def toString = resourceType.toString
}

final class ResourceIdentifier(val resourceType: ResourceType = new ResourceType(0)) extends Ordered[ResourceIdentifier] {
    override def equals(that: Any) =
        that.isInstanceOf[ResourceIdentifier] && compare(that.asInstanceOf[ResourceIdentifier]) == 0
    override def hashCode = 41 + resourceType.hashCode
    override def compare(that: ResourceIdentifier) = {
        resourceType.compare(that.resourceType)
    }
    
    override def toString = "{" + resourceType.toString + "}"
}

trait Sims3Package {
    def find(resourceIdentifier: ResourceIdentifier): Option[io.Source]
}