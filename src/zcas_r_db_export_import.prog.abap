*&---------------------------------------------------------------------*
*& Report zcas_r_db_export_import
*&---------------------------------------------------------------------*
*& Version: 10.03.2021-003
*&---------------------------------------------------------------------*
REPORT zcas_r_db_export_import.

INCLUDE zcas_r_db_export_import_top.
INCLUDE zcas_r_db_export_import_cls.
INCLUDE zcas_r_db_export_import_mod.

INITIALIZATION.
  lcl_application=>on_init( ).

START-OF-SELECTION.
  CALL SCREEN 100.
