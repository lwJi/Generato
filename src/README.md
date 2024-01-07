# Generato

## Basic

## ParseVarlist

### ParseComponent

#### SetComponent (and set map between tensor component to varlist index)

* `set components:*`
    * `set components: independent`: default, using *independent index for each var*
    * `set components: using vl_index`: using *index in varlist*
    * `set components: temporary`: no grid point index (`[ijk]`) attached (using *index in varlist* even though not used)

#### PrintComponent

* `print components initialization:*` (depend on which package we are trying to generate code for)
* `print components equation:*`
    * `print components equation: temporary*`
    * `print components equation: primary*`
    * `print components equation: adding to primary`
    * `print components equation: primary for flux`
    * `print components equation: primary with suffix`
