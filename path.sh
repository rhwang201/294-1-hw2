export BIDMAT_ROOT="fill_me_in"
export DYLD_LIBRARY_PATH="${BIDMAT_ROOT}/lib/osx64:${BIDMAT_ROOT}/lib/osx64/JCUDA5.0:/usr/local/cuda/lib:${LD_LIBRARY_PATH}"
BIDMAT_LIBS="${BIDMAT_ROOT}/BIDMat.jar:${BIDMAT_ROOT}/lib/ptplot.jar:${BIDMAT_ROOT}/lib/ptplotapplication.jar:${BIDMAT_ROOT}/lib/jhdf5.jar:${BIDMAT_ROOT}/lib/commons-math3-3.1.1.jar"
