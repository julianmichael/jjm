package jjm.instances

trait AllInstances
    extends LowerCaseStringInstances
    with DotUnitInstances

object all extends AllInstances
