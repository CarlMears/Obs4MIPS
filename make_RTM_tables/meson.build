project('RSS Radiometer Merge', 'c', 'fortran', version : '1.0.0', default_options: ['b_vscrt=static_from_buildtype'])

fortran_compiler = meson.get_compiler('fortran')

if fortran_compiler.get_id() == 'gcc'
  # Flags for gfortran (GCC)
  gfortran_flags = [
    '-ffree-form',          # Use free-form source code
    '-ffree-line-length-none', # Disable line length limits
    '-fimplicit-none',      # Require explicit declaration of all variables
    '-fbounds-check',        # Enable array bounds checking
    '-fno-range-check'     # Disable range checking (needed for NAN support)
  ]
  add_global_arguments(gfortran_flags, language : 'fortran')
  fortran_args = gfortran_flags
  package_ext = ''
else
    if get_option('buildtype') == 'debug'
        dialect = ['/nologo', '/free', '/fpp', '/debug:full']
        package_ext = '-d'
    else
        dialect = ['/nologo', '/free', '/fpp']
        package_ext = ''
    endif
    # Setup global arguments.
    bounds = '/CB'

    # make record length be in units of bytes, not 4-byte words.
    fortran_args = ['/assume:byterecl']
    add_global_arguments(dialect, language : 'fortran')
    add_global_arguments(bounds, language : 'fortran')
endif

# Find dependencies.
#nc_f_dep = dependency('netcdf-fortran'+package_ext)
#logger_dep = dependency('logger'+package_ext)
#utils_dep = dependency('utils'+package_ext)

# Add the source code.

nan_src = ['src/NAN_support_intel.f90']
date_src = ['src/date_and_time.f90']
logging_src = ['src/logging.f90']
atmos_abs_src =['src/atmos_abs_routines.f90',
               'src/dielectric_meissner_wentz.f90']
sat_constants_src = ['src/msu_constants.f90', 'src/amsu_constants.f90']
table_src = ['src/make_absorption_table_t_q.f90']
driver_src = ['src/compute_absorption_tables_amsu_dsb_driver.f90']

fortran_src = [nan_src,
       date_src,
       atmos_abs_src,
       sat_constants_src,
       table_src,]

       
#create the executeable for vapor
exe = executable('test_compute_absorption_tables_amsu_dsb', 
                  [driver_src,fortran_src],
                  install      : true, 
                  dependencies : [
                     ],
                  fortran_args : fortran_args)

py_mod = import('python')
py = py_mod.find_installation('python3.13')
py_dep = py.dependency()
netcdf_dep = dependency('netcdf-fortran', required: true)

f2py_wrapper = custom_target('f2py_wrapper',
  input : fortran_src,
  output : ['compute_absorption_tables_amsumodule.c', 'compute_absorption_tables_amsu-f2pywrappers2.f90'], # f2py often generates a second .f file
  command : [py, '-m', 'numpy.f2py', '@INPUT@', '-m', 'compute_absorption_tables_amsu', '--lower'] #,
  #'only:','compute_absorption_tables_amsu_dsb','compute_absorption_tables_amsu', ':']
) # I had to change output files to be the names of the generated files, because f2py doesn't always generate the same .f file name, 
  # and meson needs to know the exact output file names to use them in the extension module.

incdir_numpy = run_command(py,
  ['-c', 'import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py,
  ['-c', 'import numpy.f2py; print(numpy.f2py.get_include())'],
  check : true
).stdout().strip()

fortranobject_c = incdir_f2py / 'fortranobject.c'

py.extension_module('compute_absorption_tables_amsu',
  [f2py_wrapper, fortran_src,fortranobject_c],
  include_directories: [incdir_numpy, incdir_f2py],
  dependencies : [py_dep, netcdf_dep],
  install : true
)

