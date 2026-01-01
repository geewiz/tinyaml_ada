pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 15.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_test_runner" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#f755a64b#;
   pragma Export (C, u00001, "test_runnerB");
   u00002 : constant Version_32 := 16#b2cfab41#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0626cc96#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00004, "ada__strings__text_buffersB");
   u00005 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00005, "ada__strings__text_buffersS");
   u00006 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00006, "adaS");
   u00007 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00007, "ada__stringsS");
   u00008 : constant Version_32 := 16#14286b0f#;
   pragma Export (C, u00008, "systemS");
   u00009 : constant Version_32 := 16#45e1965e#;
   pragma Export (C, u00009, "system__exception_tableB");
   u00010 : constant Version_32 := 16#99031d16#;
   pragma Export (C, u00010, "system__exception_tableS");
   u00011 : constant Version_32 := 16#7fa0a598#;
   pragma Export (C, u00011, "system__soft_linksB");
   u00012 : constant Version_32 := 16#c7a3de26#;
   pragma Export (C, u00012, "system__soft_linksS");
   u00013 : constant Version_32 := 16#d0b087d0#;
   pragma Export (C, u00013, "system__secondary_stackB");
   u00014 : constant Version_32 := 16#bae33a03#;
   pragma Export (C, u00014, "system__secondary_stackS");
   u00015 : constant Version_32 := 16#57ff5296#;
   pragma Export (C, u00015, "ada__exceptionsB");
   u00016 : constant Version_32 := 16#64d9391c#;
   pragma Export (C, u00016, "ada__exceptionsS");
   u00017 : constant Version_32 := 16#85bf25f7#;
   pragma Export (C, u00017, "ada__exceptions__last_chance_handlerB");
   u00018 : constant Version_32 := 16#a028f72d#;
   pragma Export (C, u00018, "ada__exceptions__last_chance_handlerS");
   u00019 : constant Version_32 := 16#268dd43d#;
   pragma Export (C, u00019, "system__exceptionsS");
   u00020 : constant Version_32 := 16#c367aa24#;
   pragma Export (C, u00020, "system__exceptions__machineB");
   u00021 : constant Version_32 := 16#ec13924a#;
   pragma Export (C, u00021, "system__exceptions__machineS");
   u00022 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00022, "system__exceptions_debugB");
   u00023 : constant Version_32 := 16#2426335c#;
   pragma Export (C, u00023, "system__exceptions_debugS");
   u00024 : constant Version_32 := 16#36b7284e#;
   pragma Export (C, u00024, "system__img_intS");
   u00025 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00025, "ada__numericsS");
   u00026 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00026, "ada__numerics__big_numbersS");
   u00027 : constant Version_32 := 16#ee021456#;
   pragma Export (C, u00027, "system__unsigned_typesS");
   u00028 : constant Version_32 := 16#d8f6bfe7#;
   pragma Export (C, u00028, "system__storage_elementsS");
   u00029 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00029, "system__tracebackB");
   u00030 : constant Version_32 := 16#92b29fb2#;
   pragma Export (C, u00030, "system__tracebackS");
   u00031 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00031, "system__traceback_entriesB");
   u00032 : constant Version_32 := 16#dc34d483#;
   pragma Export (C, u00032, "system__traceback_entriesS");
   u00033 : constant Version_32 := 16#38e5c42b#;
   pragma Export (C, u00033, "system__traceback__symbolicB");
   u00034 : constant Version_32 := 16#140ceb78#;
   pragma Export (C, u00034, "system__traceback__symbolicS");
   u00035 : constant Version_32 := 16#179d7d28#;
   pragma Export (C, u00035, "ada__containersS");
   u00036 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00036, "ada__exceptions__tracebackB");
   u00037 : constant Version_32 := 16#26ed0985#;
   pragma Export (C, u00037, "ada__exceptions__tracebackS");
   u00038 : constant Version_32 := 16#9111f9c1#;
   pragma Export (C, u00038, "interfacesS");
   u00039 : constant Version_32 := 16#401f6fd6#;
   pragma Export (C, u00039, "interfaces__cB");
   u00040 : constant Version_32 := 16#59e2f8b5#;
   pragma Export (C, u00040, "interfaces__cS");
   u00041 : constant Version_32 := 16#a43efea2#;
   pragma Export (C, u00041, "system__parametersB");
   u00042 : constant Version_32 := 16#21bf971e#;
   pragma Export (C, u00042, "system__parametersS");
   u00043 : constant Version_32 := 16#0978786d#;
   pragma Export (C, u00043, "system__bounded_stringsB");
   u00044 : constant Version_32 := 16#63d54a16#;
   pragma Export (C, u00044, "system__bounded_stringsS");
   u00045 : constant Version_32 := 16#9f0c0c80#;
   pragma Export (C, u00045, "system__crtlS");
   u00046 : constant Version_32 := 16#799f87ee#;
   pragma Export (C, u00046, "system__dwarf_linesB");
   u00047 : constant Version_32 := 16#6c65bf08#;
   pragma Export (C, u00047, "system__dwarf_linesS");
   u00048 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00048, "ada__charactersS");
   u00049 : constant Version_32 := 16#9de61c25#;
   pragma Export (C, u00049, "ada__characters__handlingB");
   u00050 : constant Version_32 := 16#729cc5db#;
   pragma Export (C, u00050, "ada__characters__handlingS");
   u00051 : constant Version_32 := 16#cde9ea2d#;
   pragma Export (C, u00051, "ada__characters__latin_1S");
   u00052 : constant Version_32 := 16#203d5282#;
   pragma Export (C, u00052, "ada__strings__mapsB");
   u00053 : constant Version_32 := 16#6feaa257#;
   pragma Export (C, u00053, "ada__strings__mapsS");
   u00054 : constant Version_32 := 16#b451a498#;
   pragma Export (C, u00054, "system__bit_opsB");
   u00055 : constant Version_32 := 16#d9dbc733#;
   pragma Export (C, u00055, "system__bit_opsS");
   u00056 : constant Version_32 := 16#b459efcb#;
   pragma Export (C, u00056, "ada__strings__maps__constantsS");
   u00057 : constant Version_32 := 16#f9910acc#;
   pragma Export (C, u00057, "system__address_imageB");
   u00058 : constant Version_32 := 16#b5c4f635#;
   pragma Export (C, u00058, "system__address_imageS");
   u00059 : constant Version_32 := 16#219681aa#;
   pragma Export (C, u00059, "system__img_address_32S");
   u00060 : constant Version_32 := 16#0cb62028#;
   pragma Export (C, u00060, "system__img_address_64S");
   u00061 : constant Version_32 := 16#7da15eb1#;
   pragma Export (C, u00061, "system__img_unsS");
   u00062 : constant Version_32 := 16#20ec7aa3#;
   pragma Export (C, u00062, "system__ioB");
   u00063 : constant Version_32 := 16#8a6a9c40#;
   pragma Export (C, u00063, "system__ioS");
   u00064 : constant Version_32 := 16#e15ca368#;
   pragma Export (C, u00064, "system__mmapB");
   u00065 : constant Version_32 := 16#99159588#;
   pragma Export (C, u00065, "system__mmapS");
   u00066 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00066, "ada__io_exceptionsS");
   u00067 : constant Version_32 := 16#a2858c95#;
   pragma Export (C, u00067, "system__mmap__os_interfaceB");
   u00068 : constant Version_32 := 16#48fa74ab#;
   pragma Export (C, u00068, "system__mmap__os_interfaceS");
   u00069 : constant Version_32 := 16#f4289573#;
   pragma Export (C, u00069, "system__mmap__unixS");
   u00070 : constant Version_32 := 16#c04dcb27#;
   pragma Export (C, u00070, "system__os_libB");
   u00071 : constant Version_32 := 16#9143f49f#;
   pragma Export (C, u00071, "system__os_libS");
   u00072 : constant Version_32 := 16#94d23d25#;
   pragma Export (C, u00072, "system__atomic_operations__test_and_setB");
   u00073 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00073, "system__atomic_operations__test_and_setS");
   u00074 : constant Version_32 := 16#d34b112a#;
   pragma Export (C, u00074, "system__atomic_operationsS");
   u00075 : constant Version_32 := 16#553a519e#;
   pragma Export (C, u00075, "system__atomic_primitivesB");
   u00076 : constant Version_32 := 16#1cf8e0ec#;
   pragma Export (C, u00076, "system__atomic_primitivesS");
   u00077 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00077, "system__case_utilB");
   u00078 : constant Version_32 := 16#db3bbc5a#;
   pragma Export (C, u00078, "system__case_utilS");
   u00079 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00079, "system__stringsB");
   u00080 : constant Version_32 := 16#8faa6b17#;
   pragma Export (C, u00080, "system__stringsS");
   u00081 : constant Version_32 := 16#836ccd31#;
   pragma Export (C, u00081, "system__object_readerB");
   u00082 : constant Version_32 := 16#18bcfe16#;
   pragma Export (C, u00082, "system__object_readerS");
   u00083 : constant Version_32 := 16#75406883#;
   pragma Export (C, u00083, "system__val_lliS");
   u00084 : constant Version_32 := 16#838eea00#;
   pragma Export (C, u00084, "system__val_lluS");
   u00085 : constant Version_32 := 16#47d9a892#;
   pragma Export (C, u00085, "system__sparkS");
   u00086 : constant Version_32 := 16#a571a4dc#;
   pragma Export (C, u00086, "system__spark__cut_operationsB");
   u00087 : constant Version_32 := 16#629c0fb7#;
   pragma Export (C, u00087, "system__spark__cut_operationsS");
   u00088 : constant Version_32 := 16#365e21c1#;
   pragma Export (C, u00088, "system__val_utilB");
   u00089 : constant Version_32 := 16#97ef3a91#;
   pragma Export (C, u00089, "system__val_utilS");
   u00090 : constant Version_32 := 16#382ef1e7#;
   pragma Export (C, u00090, "system__exception_tracesB");
   u00091 : constant Version_32 := 16#f8b00269#;
   pragma Export (C, u00091, "system__exception_tracesS");
   u00092 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00092, "system__wch_conB");
   u00093 : constant Version_32 := 16#cd2b486c#;
   pragma Export (C, u00093, "system__wch_conS");
   u00094 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00094, "system__wch_stwB");
   u00095 : constant Version_32 := 16#e03a646d#;
   pragma Export (C, u00095, "system__wch_stwS");
   u00096 : constant Version_32 := 16#7cd63de5#;
   pragma Export (C, u00096, "system__wch_cnvB");
   u00097 : constant Version_32 := 16#cbeb821c#;
   pragma Export (C, u00097, "system__wch_cnvS");
   u00098 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00098, "system__wch_jisB");
   u00099 : constant Version_32 := 16#7e5ce036#;
   pragma Export (C, u00099, "system__wch_jisS");
   u00100 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00100, "system__soft_links__initializeB");
   u00101 : constant Version_32 := 16#ac2e8b53#;
   pragma Export (C, u00101, "system__soft_links__initializeS");
   u00102 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00102, "system__stack_checkingB");
   u00103 : constant Version_32 := 16#d3777e19#;
   pragma Export (C, u00103, "system__stack_checkingS");
   u00104 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00104, "ada__strings__utf_encodingB");
   u00105 : constant Version_32 := 16#c9e86997#;
   pragma Export (C, u00105, "ada__strings__utf_encodingS");
   u00106 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00106, "ada__strings__utf_encoding__stringsB");
   u00107 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00107, "ada__strings__utf_encoding__stringsS");
   u00108 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00108, "ada__strings__utf_encoding__wide_stringsB");
   u00109 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00109, "ada__strings__utf_encoding__wide_stringsS");
   u00110 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00110, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00111 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00111, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00112 : constant Version_32 := 16#683e3bb7#;
   pragma Export (C, u00112, "ada__tagsB");
   u00113 : constant Version_32 := 16#4ff764f3#;
   pragma Export (C, u00113, "ada__tagsS");
   u00114 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00114, "system__htableB");
   u00115 : constant Version_32 := 16#95f133e4#;
   pragma Export (C, u00115, "system__htableS");
   u00116 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00116, "system__string_hashB");
   u00117 : constant Version_32 := 16#32b4b39b#;
   pragma Export (C, u00117, "system__string_hashS");
   u00118 : constant Version_32 := 16#442efae0#;
   pragma Export (C, u00118, "aunitB");
   u00119 : constant Version_32 := 16#76cdf7c6#;
   pragma Export (C, u00119, "aunitS");
   u00120 : constant Version_32 := 16#b6c145a2#;
   pragma Export (C, u00120, "aunit__memoryB");
   u00121 : constant Version_32 := 16#c2d4cd8f#;
   pragma Export (C, u00121, "aunit__memoryS");
   u00122 : constant Version_32 := 16#bd1125e3#;
   pragma Export (C, u00122, "aunit__reporterB");
   u00123 : constant Version_32 := 16#7beb347d#;
   pragma Export (C, u00123, "aunit__reporterS");
   u00124 : constant Version_32 := 16#b228eb1e#;
   pragma Export (C, u00124, "ada__streamsB");
   u00125 : constant Version_32 := 16#613fe11c#;
   pragma Export (C, u00125, "ada__streamsS");
   u00126 : constant Version_32 := 16#05222263#;
   pragma Export (C, u00126, "system__put_imagesB");
   u00127 : constant Version_32 := 16#08866c10#;
   pragma Export (C, u00127, "system__put_imagesS");
   u00128 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00128, "ada__strings__text_buffers__utilsB");
   u00129 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00129, "ada__strings__text_buffers__utilsS");
   u00130 : constant Version_32 := 16#4f37e837#;
   pragma Export (C, u00130, "aunit__ioS");
   u00131 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00131, "ada__integer_text_ioB");
   u00132 : constant Version_32 := 16#b4dc53db#;
   pragma Export (C, u00132, "ada__integer_text_ioS");
   u00133 : constant Version_32 := 16#27ac21ac#;
   pragma Export (C, u00133, "ada__text_ioB");
   u00134 : constant Version_32 := 16#04ab031f#;
   pragma Export (C, u00134, "ada__text_ioS");
   u00135 : constant Version_32 := 16#1cacf006#;
   pragma Export (C, u00135, "interfaces__c_streamsB");
   u00136 : constant Version_32 := 16#d07279c2#;
   pragma Export (C, u00136, "interfaces__c_streamsS");
   u00137 : constant Version_32 := 16#ec2f4d1e#;
   pragma Export (C, u00137, "system__file_ioB");
   u00138 : constant Version_32 := 16#72673e49#;
   pragma Export (C, u00138, "system__file_ioS");
   u00139 : constant Version_32 := 16#c34b231e#;
   pragma Export (C, u00139, "ada__finalizationS");
   u00140 : constant Version_32 := 16#d00f339c#;
   pragma Export (C, u00140, "system__finalization_rootB");
   u00141 : constant Version_32 := 16#1e5455db#;
   pragma Export (C, u00141, "system__finalization_rootS");
   u00142 : constant Version_32 := 16#ef3c5c6f#;
   pragma Export (C, u00142, "system__finalization_primitivesB");
   u00143 : constant Version_32 := 16#927c01c5#;
   pragma Export (C, u00143, "system__finalization_primitivesS");
   u00144 : constant Version_32 := 16#e8108c8c#;
   pragma Export (C, u00144, "system__os_locksS");
   u00145 : constant Version_32 := 16#fbeae7f4#;
   pragma Export (C, u00145, "system__os_constantsS");
   u00146 : constant Version_32 := 16#9e5df665#;
   pragma Export (C, u00146, "system__file_control_blockS");
   u00147 : constant Version_32 := 16#5e511f79#;
   pragma Export (C, u00147, "ada__text_io__generic_auxB");
   u00148 : constant Version_32 := 16#d2ac8a2d#;
   pragma Export (C, u00148, "ada__text_io__generic_auxS");
   u00149 : constant Version_32 := 16#dddfe8f1#;
   pragma Export (C, u00149, "system__img_biuS");
   u00150 : constant Version_32 := 16#90812f2f#;
   pragma Export (C, u00150, "system__img_llbS");
   u00151 : constant Version_32 := 16#5eeebe35#;
   pragma Export (C, u00151, "system__img_lliS");
   u00152 : constant Version_32 := 16#e770da5d#;
   pragma Export (C, u00152, "system__img_lllbS");
   u00153 : constant Version_32 := 16#ad86ddd3#;
   pragma Export (C, u00153, "system__img_llliS");
   u00154 : constant Version_32 := 16#ed04c351#;
   pragma Export (C, u00154, "system__img_lllwS");
   u00155 : constant Version_32 := 16#ccb35a24#;
   pragma Export (C, u00155, "system__img_llwS");
   u00156 : constant Version_32 := 16#e20553c3#;
   pragma Export (C, u00156, "system__img_wiuS");
   u00157 : constant Version_32 := 16#aa0160a2#;
   pragma Export (C, u00157, "system__val_intS");
   u00158 : constant Version_32 := 16#5da6ebca#;
   pragma Export (C, u00158, "system__val_unsS");
   u00159 : constant Version_32 := 16#a5fee39b#;
   pragma Export (C, u00159, "system__val_llliS");
   u00160 : constant Version_32 := 16#1e4a2c79#;
   pragma Export (C, u00160, "system__val_llluS");
   u00161 : constant Version_32 := 16#e99cd447#;
   pragma Export (C, u00161, "aunit__optionsS");
   u00162 : constant Version_32 := 16#e9d6512d#;
   pragma Export (C, u00162, "aunit__test_filtersB");
   u00163 : constant Version_32 := 16#9a67cba8#;
   pragma Export (C, u00163, "aunit__test_filtersS");
   u00164 : constant Version_32 := 16#6e9501f4#;
   pragma Export (C, u00164, "aunit__simple_test_casesB");
   u00165 : constant Version_32 := 16#5a323d45#;
   pragma Export (C, u00165, "aunit__simple_test_casesS");
   u00166 : constant Version_32 := 16#f1db610e#;
   pragma Export (C, u00166, "aunit__assertionsB");
   u00167 : constant Version_32 := 16#f6326ff1#;
   pragma Export (C, u00167, "aunit__assertionsS");
   u00168 : constant Version_32 := 16#df831941#;
   pragma Export (C, u00168, "ada_containers__aunit_listsB");
   u00169 : constant Version_32 := 16#c8d9569a#;
   pragma Export (C, u00169, "ada_containers__aunit_listsS");
   u00170 : constant Version_32 := 16#11329e00#;
   pragma Export (C, u00170, "ada_containersS");
   u00171 : constant Version_32 := 16#9b1c7ff2#;
   pragma Export (C, u00171, "aunit__memory__utilsB");
   u00172 : constant Version_32 := 16#fb2f6c57#;
   pragma Export (C, u00172, "aunit__memory__utilsS");
   u00173 : constant Version_32 := 16#b891ec3b#;
   pragma Export (C, u00173, "aunit__test_resultsB");
   u00174 : constant Version_32 := 16#c2a99f30#;
   pragma Export (C, u00174, "aunit__test_resultsS");
   u00175 : constant Version_32 := 16#737bafa8#;
   pragma Export (C, u00175, "aunit__time_measureB");
   u00176 : constant Version_32 := 16#eb2e5d34#;
   pragma Export (C, u00176, "aunit__time_measureS");
   u00177 : constant Version_32 := 16#96a20755#;
   pragma Export (C, u00177, "ada__strings__fixedB");
   u00178 : constant Version_32 := 16#11b694ce#;
   pragma Export (C, u00178, "ada__strings__fixedS");
   u00179 : constant Version_32 := 16#b40d9bf2#;
   pragma Export (C, u00179, "ada__strings__searchB");
   u00180 : constant Version_32 := 16#97fe4a15#;
   pragma Export (C, u00180, "ada__strings__searchS");
   u00181 : constant Version_32 := 16#78511131#;
   pragma Export (C, u00181, "ada__calendarB");
   u00182 : constant Version_32 := 16#c907a168#;
   pragma Export (C, u00182, "ada__calendarS");
   u00183 : constant Version_32 := 16#d172d809#;
   pragma Export (C, u00183, "system__os_primitivesB");
   u00184 : constant Version_32 := 16#13d50ef9#;
   pragma Export (C, u00184, "system__os_primitivesS");
   u00185 : constant Version_32 := 16#6b6cea8f#;
   pragma Export (C, u00185, "aunit__testsS");
   u00186 : constant Version_32 := 16#ae5b86de#;
   pragma Export (C, u00186, "system__pool_globalB");
   u00187 : constant Version_32 := 16#a07c1f1e#;
   pragma Export (C, u00187, "system__pool_globalS");
   u00188 : constant Version_32 := 16#0ddbd91f#;
   pragma Export (C, u00188, "system__memoryB");
   u00189 : constant Version_32 := 16#0cbcf715#;
   pragma Export (C, u00189, "system__memoryS");
   u00190 : constant Version_32 := 16#35d6ef80#;
   pragma Export (C, u00190, "system__storage_poolsB");
   u00191 : constant Version_32 := 16#8e431254#;
   pragma Export (C, u00191, "system__storage_poolsS");
   u00192 : constant Version_32 := 16#b5988c27#;
   pragma Export (C, u00192, "gnatS");
   u00193 : constant Version_32 := 16#f299cac9#;
   pragma Export (C, u00193, "gnat__source_infoS");
   u00194 : constant Version_32 := 16#931654a0#;
   pragma Export (C, u00194, "gnat__tracebackB");
   u00195 : constant Version_32 := 16#c46c6d9b#;
   pragma Export (C, u00195, "gnat__tracebackS");
   u00196 : constant Version_32 := 16#2086345e#;
   pragma Export (C, u00196, "gnat__traceback__symbolicS");
   u00197 : constant Version_32 := 16#756a1fdd#;
   pragma Export (C, u00197, "system__stream_attributesB");
   u00198 : constant Version_32 := 16#a8236f45#;
   pragma Export (C, u00198, "system__stream_attributesS");
   u00199 : constant Version_32 := 16#1c617d0b#;
   pragma Export (C, u00199, "system__stream_attributes__xdrB");
   u00200 : constant Version_32 := 16#e4218e58#;
   pragma Export (C, u00200, "system__stream_attributes__xdrS");
   u00201 : constant Version_32 := 16#d71ab463#;
   pragma Export (C, u00201, "system__fat_fltS");
   u00202 : constant Version_32 := 16#f128bd6e#;
   pragma Export (C, u00202, "system__fat_lfltS");
   u00203 : constant Version_32 := 16#8bf81384#;
   pragma Export (C, u00203, "system__fat_llfS");
   u00204 : constant Version_32 := 16#b61e55fe#;
   pragma Export (C, u00204, "aunit__reporter__textB");
   u00205 : constant Version_32 := 16#1676cc84#;
   pragma Export (C, u00205, "aunit__reporter__textS");
   u00206 : constant Version_32 := 16#afdc38b2#;
   pragma Export (C, u00206, "system__arith_64B");
   u00207 : constant Version_32 := 16#509fabdd#;
   pragma Export (C, u00207, "system__arith_64S");
   u00208 : constant Version_32 := 16#b0a247c9#;
   pragma Export (C, u00208, "system__exn_intS");
   u00209 : constant Version_32 := 16#d79db92c#;
   pragma Export (C, u00209, "system__return_stackS");
   u00210 : constant Version_32 := 16#4d723195#;
   pragma Export (C, u00210, "aunit__runB");
   u00211 : constant Version_32 := 16#dc46304b#;
   pragma Export (C, u00211, "aunit__runS");
   u00212 : constant Version_32 := 16#276e73f2#;
   pragma Export (C, u00212, "aunit__test_suitesB");
   u00213 : constant Version_32 := 16#50924664#;
   pragma Export (C, u00213, "aunit__test_suitesS");
   u00214 : constant Version_32 := 16#4007cd01#;
   pragma Export (C, u00214, "tinyaml_testsB");
   u00215 : constant Version_32 := 16#64d90a3d#;
   pragma Export (C, u00215, "tinyaml_testsS");
   u00216 : constant Version_32 := 16#a947be8f#;
   pragma Export (C, u00216, "tinyamlB");
   u00217 : constant Version_32 := 16#c27301f7#;
   pragma Export (C, u00217, "tinyamlS");
   u00218 : constant Version_32 := 16#4259a79c#;
   pragma Export (C, u00218, "ada__strings__unboundedB");
   u00219 : constant Version_32 := 16#b40332b4#;
   pragma Export (C, u00219, "ada__strings__unboundedS");
   u00220 : constant Version_32 := 16#52627794#;
   pragma Export (C, u00220, "system__atomic_countersB");
   u00221 : constant Version_32 := 16#c83084cc#;
   pragma Export (C, u00221, "system__atomic_countersS");
   u00222 : constant Version_32 := 16#a24f61e2#;
   pragma Export (C, u00222, "tinyaml__lexer_testsB");
   u00223 : constant Version_32 := 16#925e1f4f#;
   pragma Export (C, u00223, "tinyaml__lexer_testsS");
   u00224 : constant Version_32 := 16#690693e0#;
   pragma Export (C, u00224, "system__storage_pools__subpoolsB");
   u00225 : constant Version_32 := 16#23a252fc#;
   pragma Export (C, u00225, "system__storage_pools__subpoolsS");
   u00226 : constant Version_32 := 16#3676fd0b#;
   pragma Export (C, u00226, "system__storage_pools__subpools__finalizationB");
   u00227 : constant Version_32 := 16#54c94065#;
   pragma Export (C, u00227, "system__storage_pools__subpools__finalizationS");
   u00228 : constant Version_32 := 16#c2b4932a#;
   pragma Export (C, u00228, "tinyaml__lexerB");
   u00229 : constant Version_32 := 16#3e9bf4a2#;
   pragma Export (C, u00229, "tinyaml__lexerS");
   u00230 : constant Version_32 := 16#4c5eed8b#;
   pragma Export (C, u00230, "aunit__test_casesB");
   u00231 : constant Version_32 := 16#1aa5f28d#;
   pragma Export (C, u00231, "aunit__test_casesS");
   u00232 : constant Version_32 := 16#25bcafd3#;
   pragma Export (C, u00232, "tinyaml__parser_testsB");
   u00233 : constant Version_32 := 16#aeae59ab#;
   pragma Export (C, u00233, "tinyaml__parser_testsS");
   u00234 : constant Version_32 := 16#32bfbcad#;
   pragma Export (C, u00234, "tinyaml__documentsB");
   u00235 : constant Version_32 := 16#5ae76fd2#;
   pragma Export (C, u00235, "tinyaml__documentsS");
   u00236 : constant Version_32 := 16#bf83ff50#;
   pragma Export (C, u00236, "tinyaml__nodesB");
   u00237 : constant Version_32 := 16#5348d7e9#;
   pragma Export (C, u00237, "tinyaml__nodesS");
   u00238 : constant Version_32 := 16#e259c480#;
   pragma Export (C, u00238, "system__assertionsB");
   u00239 : constant Version_32 := 16#322b1494#;
   pragma Export (C, u00239, "system__assertionsS");
   u00240 : constant Version_32 := 16#8b2c6428#;
   pragma Export (C, u00240, "ada__assertionsB");
   u00241 : constant Version_32 := 16#cc3ec2fd#;
   pragma Export (C, u00241, "ada__assertionsS");
   u00242 : constant Version_32 := 16#6e3816c9#;
   pragma Export (C, u00242, "tinyaml__nodes__mapB");
   u00243 : constant Version_32 := 16#d7a0eae5#;
   pragma Export (C, u00243, "tinyaml__nodes__mapS");
   u00244 : constant Version_32 := 16#a83b2700#;
   pragma Export (C, u00244, "tinyaml__nodes__sequenceB");
   u00245 : constant Version_32 := 16#61b272df#;
   pragma Export (C, u00245, "tinyaml__nodes__sequenceS");
   u00246 : constant Version_32 := 16#c3b32edd#;
   pragma Export (C, u00246, "ada__containers__helpersB");
   u00247 : constant Version_32 := 16#444c93c2#;
   pragma Export (C, u00247, "ada__containers__helpersS");
   u00248 : constant Version_32 := 16#f4ca97ce#;
   pragma Export (C, u00248, "ada__containers__red_black_treesS");
   u00249 : constant Version_32 := 16#b3f7543e#;
   pragma Export (C, u00249, "system__strings__stream_opsB");
   u00250 : constant Version_32 := 16#46dadf54#;
   pragma Export (C, u00250, "system__strings__stream_opsS");
   u00251 : constant Version_32 := 16#30690ef5#;
   pragma Export (C, u00251, "tinyaml__nodes__navigationB");
   u00252 : constant Version_32 := 16#99e271c5#;
   pragma Export (C, u00252, "tinyaml__nodes__navigationS");
   u00253 : constant Version_32 := 16#1f91cd5f#;
   pragma Export (C, u00253, "tinyaml__nodes__scalarB");
   u00254 : constant Version_32 := 16#152816d9#;
   pragma Export (C, u00254, "tinyaml__nodes__scalarS");
   u00255 : constant Version_32 := 16#fd6581dd#;
   pragma Export (C, u00255, "tinyaml__parserB");
   u00256 : constant Version_32 := 16#8fe5c36a#;
   pragma Export (C, u00256, "tinyaml__parserS");
   u00257 : constant Version_32 := 16#9351de22#;
   pragma Export (C, u00257, "system__taskingB");
   u00258 : constant Version_32 := 16#82c55864#;
   pragma Export (C, u00258, "system__taskingS");
   u00259 : constant Version_32 := 16#9022318b#;
   pragma Export (C, u00259, "system__task_primitivesS");
   u00260 : constant Version_32 := 16#5c897da3#;
   pragma Export (C, u00260, "system__os_interfaceB");
   u00261 : constant Version_32 := 16#5bee0e11#;
   pragma Export (C, u00261, "system__os_interfaceS");
   u00262 : constant Version_32 := 16#fc760bf8#;
   pragma Export (C, u00262, "system__linuxS");
   u00263 : constant Version_32 := 16#cf8f5d61#;
   pragma Export (C, u00263, "system__task_primitives__operationsB");
   u00264 : constant Version_32 := 16#ef492e06#;
   pragma Export (C, u00264, "system__task_primitives__operationsS");
   u00265 : constant Version_32 := 16#900fbd22#;
   pragma Export (C, u00265, "system__interrupt_managementB");
   u00266 : constant Version_32 := 16#de9ae4af#;
   pragma Export (C, u00266, "system__interrupt_managementS");
   u00267 : constant Version_32 := 16#73dc29bf#;
   pragma Export (C, u00267, "system__multiprocessorsB");
   u00268 : constant Version_32 := 16#2c84f47c#;
   pragma Export (C, u00268, "system__multiprocessorsS");
   u00269 : constant Version_32 := 16#4ee862d1#;
   pragma Export (C, u00269, "system__task_infoB");
   u00270 : constant Version_32 := 16#cf451a05#;
   pragma Export (C, u00270, "system__task_infoS");
   u00271 : constant Version_32 := 16#45653325#;
   pragma Export (C, u00271, "system__tasking__debugB");
   u00272 : constant Version_32 := 16#104d3ae8#;
   pragma Export (C, u00272, "system__tasking__debugS");
   u00273 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00273, "system__concat_2B");
   u00274 : constant Version_32 := 16#a1d318f8#;
   pragma Export (C, u00274, "system__concat_2S");
   u00275 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00275, "system__concat_3B");
   u00276 : constant Version_32 := 16#9e5272ad#;
   pragma Export (C, u00276, "system__concat_3S");
   u00277 : constant Version_32 := 16#3066cab0#;
   pragma Export (C, u00277, "system__stack_usageB");
   u00278 : constant Version_32 := 16#4a68f31e#;
   pragma Export (C, u00278, "system__stack_usageS");
   u00279 : constant Version_32 := 16#1bb69cc0#;
   pragma Export (C, u00279, "tinyaml__validation_testsB");
   u00280 : constant Version_32 := 16#68a97def#;
   pragma Export (C, u00280, "tinyaml__validation_testsS");
   u00281 : constant Version_32 := 16#368b1889#;
   pragma Export (C, u00281, "tinyaml__schemasB");
   u00282 : constant Version_32 := 16#6238b6cb#;
   pragma Export (C, u00282, "tinyaml__schemasS");
   u00283 : constant Version_32 := 16#2ac92e78#;
   pragma Export (C, u00283, "tinyaml__schemas__mapB");
   u00284 : constant Version_32 := 16#4dac805e#;
   pragma Export (C, u00284, "tinyaml__schemas__mapS");
   u00285 : constant Version_32 := 16#3553bec6#;
   pragma Export (C, u00285, "tinyaml__schemas__anyS");
   u00286 : constant Version_32 := 16#1d468b88#;
   pragma Export (C, u00286, "tinyaml__schemas__boolB");
   u00287 : constant Version_32 := 16#6f50c939#;
   pragma Export (C, u00287, "tinyaml__schemas__boolS");
   u00288 : constant Version_32 := 16#9afc96ca#;
   pragma Export (C, u00288, "tinyaml__schemas__fltB");
   u00289 : constant Version_32 := 16#71d8e5e6#;
   pragma Export (C, u00289, "tinyaml__schemas__fltS");
   u00290 : constant Version_32 := 16#2611fc39#;
   pragma Export (C, u00290, "system__val_lfltS");
   u00291 : constant Version_32 := 16#0f79a52f#;
   pragma Export (C, u00291, "system__exn_lfltS");
   u00292 : constant Version_32 := 16#1b28662b#;
   pragma Export (C, u00292, "system__float_controlB");
   u00293 : constant Version_32 := 16#f4d42833#;
   pragma Export (C, u00293, "system__float_controlS");
   u00294 : constant Version_32 := 16#b82039c7#;
   pragma Export (C, u00294, "system__powten_lfltS");
   u00295 : constant Version_32 := 16#ce0c311e#;
   pragma Export (C, u00295, "tinyaml__schemas__intB");
   u00296 : constant Version_32 := 16#a721c890#;
   pragma Export (C, u00296, "tinyaml__schemas__intS");
   u00297 : constant Version_32 := 16#f7fd9b57#;
   pragma Export (C, u00297, "tinyaml__schemas__constraintsS");
   u00298 : constant Version_32 := 16#d8150ade#;
   pragma Export (C, u00298, "tinyaml__schemas__constraints__range_constraintB");
   u00299 : constant Version_32 := 16#3a739acc#;
   pragma Export (C, u00299, "tinyaml__schemas__constraints__range_constraintS");
   u00300 : constant Version_32 := 16#9f7f51e2#;
   pragma Export (C, u00300, "tinyaml__schemas__strB");
   u00301 : constant Version_32 := 16#f37fb44e#;
   pragma Export (C, u00301, "tinyaml__schemas__strS");
   u00302 : constant Version_32 := 16#c857814a#;
   pragma Export (C, u00302, "tinyaml__schemas__enumB");
   u00303 : constant Version_32 := 16#91899e9c#;
   pragma Export (C, u00303, "tinyaml__schemas__enumS");
   u00304 : constant Version_32 := 16#1bca34b5#;
   pragma Export (C, u00304, "tinyaml__schemas__seqB");
   u00305 : constant Version_32 := 16#81b642f9#;
   pragma Export (C, u00305, "tinyaml__schemas__seqS");
   u00306 : constant Version_32 := 16#ea7b6a3b#;
   pragma Export (C, u00306, "tinyaml__schemas__preludeS");
   u00307 : constant Version_32 := 16#6c0934a3#;
   pragma Export (C, u00307, "tinyaml__validationB");
   u00308 : constant Version_32 := 16#db4b7fa7#;
   pragma Export (C, u00308, "tinyaml__validationS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.float_control%s
   --  system.float_control%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.os_primitives%s
   --  system.os_primitives%b
   --  system.powten_lflt%s
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.img_address_32%s
   --  system.img_address_64%s
   --  system.return_stack%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_llb%s
   --  system.img_lllb%s
   --  system.img_lllw%s
   --  system.img_llw%s
   --  system.img_wiu%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.exn_lflt%s
   --  system.traceback%s
   --  system.traceback%b
   --  ada.characters.handling%s
   --  system.atomic_operations.test_and_set%s
   --  system.case_util%s
   --  system.os_lib%s
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_lli%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  system.bounded_strings%s
   --  system.bounded_strings%b
   --  system.case_util%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.containers%s
   --  ada.io_exceptions%s
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  ada.strings%s
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  ada.characters.handling%b
   --  system.atomic_operations.test_and_set%b
   --  system.exception_traces%s
   --  system.exception_traces%b
   --  system.img_int%s
   --  system.img_uns%s
   --  system.memory%s
   --  system.memory%b
   --  system.mmap%s
   --  system.mmap.os_interface%s
   --  system.mmap%b
   --  system.mmap.unix%s
   --  system.mmap.os_interface%b
   --  system.object_reader%s
   --  system.object_reader%b
   --  system.dwarf_lines%s
   --  system.dwarf_lines%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.fixed%s
   --  ada.strings.fixed%b
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  gnat%s
   --  gnat.source_info%s
   --  system.arith_64%s
   --  system.arith_64%b
   --  system.atomic_counters%s
   --  system.atomic_counters%b
   --  system.fat_flt%s
   --  system.fat_lflt%s
   --  system.fat_llf%s
   --  system.linux%s
   --  system.multiprocessors%s
   --  system.multiprocessors%b
   --  system.os_constants%s
   --  system.os_locks%s
   --  system.finalization_primitives%s
   --  system.finalization_primitives%b
   --  system.os_interface%s
   --  system.os_interface%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.containers.helpers%s
   --  ada.containers.helpers%b
   --  ada.containers.red_black_trees%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.stack_usage%s
   --  system.stack_usage%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.storage_pools.subpools%b
   --  system.stream_attributes%s
   --  system.stream_attributes.xdr%s
   --  system.stream_attributes.xdr%b
   --  system.stream_attributes%b
   --  ada.strings.unbounded%s
   --  ada.strings.unbounded%b
   --  system.task_info%s
   --  system.task_info%b
   --  system.task_primitives%s
   --  system.interrupt_management%s
   --  system.interrupt_management%b
   --  system.val_lflt%s
   --  system.val_lllu%s
   --  system.val_llli%s
   --  system.val_uns%s
   --  system.val_int%s
   --  ada.calendar%s
   --  ada.calendar%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  gnat.traceback%s
   --  gnat.traceback%b
   --  gnat.traceback.symbolic%s
   --  system.assertions%s
   --  system.assertions%b
   --  system.exn_int%s
   --  system.img_lli%s
   --  system.tasking%s
   --  system.task_primitives.operations%s
   --  system.tasking.debug%s
   --  system.tasking.debug%b
   --  system.task_primitives.operations%b
   --  system.tasking%b
   --  system.img_llli%s
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.strings.stream_ops%s
   --  system.strings.stream_ops%b
   --  ada_containers%s
   --  tinyaml%s
   --  tinyaml%b
   --  aunit%s
   --  aunit.memory%s
   --  aunit.memory%b
   --  aunit%b
   --  aunit.io%s
   --  aunit.memory.utils%s
   --  aunit.memory.utils%b
   --  ada_containers.aunit_lists%s
   --  ada_containers.aunit_lists%b
   --  aunit.tests%s
   --  aunit.time_measure%s
   --  aunit.time_measure%b
   --  aunit.test_results%s
   --  aunit.test_results%b
   --  aunit.assertions%s
   --  aunit.assertions%b
   --  aunit.test_filters%s
   --  aunit.options%s
   --  aunit.simple_test_cases%s
   --  aunit.simple_test_cases%b
   --  aunit.test_filters%b
   --  aunit.reporter%s
   --  aunit.reporter%b
   --  aunit.reporter.text%s
   --  aunit.reporter.text%b
   --  aunit.test_cases%s
   --  aunit.test_cases%b
   --  aunit.test_suites%s
   --  aunit.test_suites%b
   --  aunit.run%s
   --  aunit.run%b
   --  tinyaml.lexer%s
   --  tinyaml.lexer%b
   --  tinyaml.lexer_tests%s
   --  tinyaml.lexer_tests%b
   --  tinyaml.nodes%s
   --  tinyaml.nodes.map%s
   --  tinyaml.nodes.map%b
   --  tinyaml.nodes.sequence%s
   --  tinyaml.nodes.sequence%b
   --  tinyaml.nodes%b
   --  tinyaml.documents%s
   --  tinyaml.documents%b
   --  tinyaml.nodes.scalar%s
   --  tinyaml.nodes.scalar%b
   --  tinyaml.nodes.navigation%s
   --  tinyaml.nodes.navigation%b
   --  tinyaml.parser%s
   --  tinyaml.parser%b
   --  tinyaml.parser_tests%s
   --  tinyaml.parser_tests%b
   --  tinyaml.schemas%s
   --  tinyaml.schemas.any%s
   --  tinyaml.schemas.bool%s
   --  tinyaml.schemas.bool%b
   --  tinyaml.schemas.constraints%s
   --  tinyaml.schemas.constraints.range_constraint%s
   --  tinyaml.schemas.constraints.range_constraint%b
   --  tinyaml.schemas.enum%s
   --  tinyaml.schemas.enum%b
   --  tinyaml.schemas.flt%s
   --  tinyaml.schemas.flt%b
   --  tinyaml.schemas.int%s
   --  tinyaml.schemas.int%b
   --  tinyaml.schemas.seq%s
   --  tinyaml.schemas.seq%b
   --  tinyaml.schemas.map%s
   --  tinyaml.schemas%b
   --  tinyaml.schemas.str%s
   --  tinyaml.schemas.str%b
   --  tinyaml.schemas.map%b
   --  tinyaml.schemas.prelude%s
   --  tinyaml.validation%s
   --  tinyaml.validation%b
   --  tinyaml.validation_tests%s
   --  tinyaml.validation_tests%b
   --  tinyaml_tests%s
   --  tinyaml_tests%b
   --  test_runner%b
   --  END ELABORATION ORDER

end ada_main;
