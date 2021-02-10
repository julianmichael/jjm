package jjm.instances

trait AllInstances
    extends LowerCaseStringInstances
    with DotUnitInstances
    with RecordInstances

object all extends AllInstances
