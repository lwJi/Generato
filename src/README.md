# Generato

## Basic

## ParseVarlist

### ParseComponent

#### 1. `set components*` $\rightarrow$ `SetComponent[]` (and set map between tensor component to varlist index)

* `set components: independent`: default, using *independent index for each var*

* `set components: using vl_index`: using *index in varlist*

* Options
    - `addgpidx`: no grid point index (`[ijk]`) attached

#### 2. `print components*` $\rightarrow$ `PrintComponent[]`

##### 2a. `print components initialization*` $\rightarrow$ `PrintComponentInitialization[]` (depend on which package we are trying to generate code for)

* Options

##### 2b. `print components equation*` $\rightarrow$ `PrintComponentEquation[]`

* `print components equation: temporary*`

* `print components equation: primary*`

* `print components equation: adding to primary`

* `print components equation: primary for flux`

* `print components equation: primary with suffix`

* Options
