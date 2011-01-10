#!/bin/ksh

######################################################################################
######################################################################################
###
### 1) machine dependant functions : each machine has its own functions
###
######################################################################################
######################################################################################

case $MACHINE in

##############################################################################

    'jade')

# jade version: all is supposed to be in WORKDIR
 
# CHKDIRG : check the existence of a directory on the Storage machine.
chkdirg() { if [ ! -d $WORKDIR/$1 ] ; then mkdir $WORKDIR/$1 ; fi ; }

# CHKFILE : check if a file exists on the storage machine, return present or absent.
chkfile() {  if [ -f $WORKDIR/$1 ] ; then echo present ;\
                       else echo absent ; fi  ; }

# CHKDIR  : check the existence of a directory. Create it if not present
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }

# RAPATRIE : get file $1 from directory $2   to local dir, and name it $3
#            directory $2 is normally on the Storage machine
rapatrie() { ln -sf $WORKDIR/$2/$1 $3 ; }

# RAPATRIE_5d : get 5-day averaged file for a given grid $1 from directory $2 
#               directory $2 is normally on the Storage machine
rapatrie_5d() { for f  in $WORKDIR/$2/*_$1.nc    ; do
                 file=`basename $f`
                 ln -sf $WORKDIR/$2/$file . 
                done  ; }

# EXPATRIE : put file $1 to directory $2 with name $3
expatrie() { cp $1 $WORKDIR/$2/$3 ; }

# additional functions used in the pre/post job launch
cp_remote2local () { scp $USER@${login_node}:$2/$1 ./$3 ; }
cp_local2remote () { scp ./$1  $USER@${login_node}:$2/$3 ; }

# CHKDIRW : check the existence of a dir. on the web site. Create it if not present
chkdirw() { ssh drakkar@meolipc.hmg.inpg.fr " if [ ! -d DRAKKAR/$1 ] ; \
            then mkdir DRAKKAR/$1 ; fi " ; }

echo "functions for machine $MACHINE successfully loaded" ;;

##############################################################################

    'desktop')

# desktop version: all is supposed to be in SDIR of the local machine
 
# CHKDIRG : check the existence of a directory on the Storage machine.
chkdirg() { if [ ! -d $SDIR/$1 ] ; then mkdir $SDIR/$1 ; fi ; }

# CHKFILE : check if a file exists on the storage machine, return present or absent.
chkfile() {  if [ -f $SDIR/$1 ] ; then echo present ;\
                       else echo absent ; fi  ; }

# CHKDIR  : check the existence of a directory. Create it if not present
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }

# RAPATRIE : get file $1 from directory $2   to local dir, and name it $3
#            directory $2 is normally on the Storage machine
rapatrie() { ln -sf $SDIR/$2/$1 $3 ; }

# RAPATRIE_5d : get 5-day averaged file for a given grid $1 from directory $2 
#               directory $2 is normally on the Storage machine
rapatrie_5d() { for f  in $SDIR/$2/*_$1.nc    ; do
                 file=`basename $f`
                 ln -sf $SDIR/$2/$file . 
                done  ; }

# EXPATRIE : put file $1 to directory $2 with name $3
expatrie() { cp $1 $SDIR/$2/$3 ; }

# CHKDIRW : check the existence of a dir. on the web site. Create it if not present
chkdirw() { ssh drakkar@meolipc.hmg.inpg.fr " if [ ! -d DRAKKAR/$1 ] ; \
            then mkdir DRAKKAR/$1 ; fi " ; }

echo "functions for machine $MACHINE successfully loaded" ;;

##############################################################################

    'ulam' )

# CHKDIRG : check the existence of a directory on the Storage machine.
chkdirg() { if [ ! -d $SDIR/$1 ] ; then mkdir $SDIR/$1 ; fi  ; }

# CHKFILE : check if a file exists on the storage machine, return present or absent.
chkfile() {  if [ -f $SDIR/$1 ] ; then echo present ;\
                       else echo absent ; fi  ; }

# CHKDIR  : check the existence of a directory. Create it if not present
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }

# RAPATRIE : get file $1 from directory $2   to local dir, and name it $3
#            directory $2 is normally on the Storage machine
rapatrie() { if [ ! -f $3 ] ; then mfget -u $REMOTE_USER $2/$1 $3 ; else echo $3 is already \
            downloaded ; fi ; }

# RAPATRIE_5d : get 5-day averaged file for a given grid $1 from directory $2 
#               directory $2 is normally on the Storage machine
rapatrie_5d() { for f  in $SDIR/$2/*_$1.nc    ; do
                 file=`basename $f`
                 ln -sf $SDIR/$2/$file . 
                done  ; }

# EXPATRIE : put file $1 to directory $2 with name $3
expatrie() { mfput -u $REMOTE_USER $1 $2/$3 ; }

# CHKDIRW : check the existence of a dir. on the web site. Create it if not present
chkdirw() { ssh drakkar@meolipc.hmg.inpg.fr " if [ ! -d DRAKKAR/$1 ] ; \
            then mkdir DRAKKAR/$1 ; fi " ; }

echo "functions for machine $MACHINE successfully loaded" ;;

##############################################################################

    'vargas')

# CHKDIRG : check the existence of a directory on the Storage machine.
chkdirg() { rsh gaya -l $REMOTE_USER " if [ ! -d $1 ] ; then mkdir $1 ; fi " ; }

# CHKFILE : check if a file exists on the storage machine, return present or absent.
chkfile() { rsh gaya -l $REMOTE_USER " if [ -f $1 ] ; then echo present ;\
                       else echo absent ; fi " ; }

# CHKDIR  : check the existence of a directory. Create it if not present
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }

# RAPATRIE : get file $1 from directory $2   to local dir, and name it $3
#            directory $2 is normally on the Storage machine
rapatrie() { if [ ! -f $3 ] ; then mfget -u $REMOTE_USER $2/$1 $3 ; else echo $3 is already \
            downloaded ; fi ; }

# RAPATRIE_5d : get 5-day averaged file for a given grid $1 from directory $2 
#               directory $2 is normally on the Storage machine
rapatrie_5d() { for f  in $( rsh gaya -l $REMOTE_USER ls $2/*_$1.nc ) ; do
                 file=`basename $f`
                 mfget -u $REMOTE_USER $2/$file .
                done  ; }

# EXPATRIE : put file $1 to directory $2 with name $3
expatrie() { mfput -u $REMOTE_USER $1 $2/$3 ; }

# CHKDIRW : check the existence of a dir. on the web site. Create it if not present
chkdirw() { ssh drakkar@meolipc.hmg.inpg.fr " if [ ! -d DRAKKAR/$1 ] ; \
            then mkdir DRAKKAR/$1 ; fi " ; }

echo "functions for machine $MACHINE successfully loaded" ;;

    *)
    echo available machines are jade desktop ulam vargas ; exit 1 ;;
esac

######################################################################################
######################################################################################
###
### 2) common functions : only shell functions indep of machine
###
######################################################################################
######################################################################################

######################################################################################
# find_model_level : return the level corresponding to dep (m) by superior value

# usage : find_model_level depth_dim_name depth file
  find_model_level() { ncks -H -F -C -v $1 $3 | \
           sed -e 's/(/ /' -e 's/)/ /' -e 's/=/ /' | \
           awk '{if ( $3 > dep ) {print $2 ; nextfile }   }' dep=$2  ; }


######################################################################################
# interpolation_vertical depth_dim_name target_depth var_to_interpolate file

  interpolation_vertical() { if [ $# != 4 ] ; then exit 1 ; fi ; \
                             level_sup=$( find_model_level $1 $2 $4 ) ; \
                             level_inf=$(( $level_sup - 1 )) ; \
                             depth_sup=$( ncks -H -F -C -v $1 $4 | sed -e 's/(/ /' -e 's/)/ /' -e 's/=/ /' | \
                                          grep " $level_sup " | awk '{print $NF}' ) ; \
                             depth_inf=$( ncks -H -F -C -v $1 $4 | sed -e 's/(/ /' -e 's/)/ /' -e 's/=/ /' | \
                                          grep " $level_inf " | awk '{print $NF}' ) ; \
                             vit_sup=$( ncks -H -F -C -v $3 $4 | sed -e 's/(/ /g' -e 's/)/ /g' -e 's/=/ /g' | \
                                        grep " $level_sup " | awk '{print $NF}' ) ; \
                             vit_inf=$( ncks -H -F -C -v $3 $4 | sed -e 's/(/ /g' -e 's/)/ /g' -e 's/=/ /g' | \
                                        grep " $level_inf " | awk '{print $NF}' ) ; \
                             up=$(   echo "$2 $depth_inf"       |  awk '{print $1 - $2}' ) ; \
                             down=$( echo "$depth_sup $depth_inf" |  awk '{print $1 - $2}' ) ; \
                             diff=$( echo "$vit_sup $vit_inf"     |  awk '{print $1 - $2}' ) ; \
                             interp=$(echo "$vit_inf $diff $up $down" | awk '{print $1 + ( $2 * ( $3 / $4 )) }') ; \
                             echo $interp ; }

######################################################################################
# copy_to_web : copy the time series figures to the DRAKKAR website
# usage : copy_to_web file
copy_to_web() {
          ssh meolipc.hmg.inpg.fr -l drakkar " if [ ! -d DRAKKAR/$CONFIG ] ; then mkdir DRAKKAR/$CONFIG ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE ; fi "
          ssh meolipc.hmg.inpg.fr -l drakkar \
         " if [ ! -d DRAKKAR/$CONFIG/$CONFCASE/TIME_SERIES ] ; then mkdir DRAKKAR/$CONFIG/$CONFCASE/TIME_SERIES ; fi "
          scp $1 drakkar@meolipc.hmg.inpg.fr:DRAKKAR/$CONFIG/${CONFCASE}/TIME_SERIES/$1 ;}

