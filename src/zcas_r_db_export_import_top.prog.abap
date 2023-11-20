*&---------------------------------------------------------------------*
*& Include zcas_r_db_export_import_top
*&---------------------------------------------------------------------*
CONTROLS: main_tabstrip TYPE TABSTRIP.

TYPES: BEGIN OF s_table_dataset,
         param  TYPE string,
         value  TYPE string,
         is_key TYPE abap_bool,
       END OF s_table_dataset,
       tt_table_dataset TYPE TABLE OF s_table_dataset WITH KEY param.

TYPES: BEGIN OF s_table_content,
         dataset TYPE tt_table_dataset,
       END OF s_table_content,
       tt_table_content TYPE TABLE OF s_table_content WITH DEFAULT KEY.

TYPES: BEGIN OF s_table_header,
         fieldname TYPE fieldname,
       END OF s_table_header,
       tt_table_header TYPE TABLE OF s_table_header WITH KEY fieldname.

TYPES: BEGIN OF s_table,
         name    TYPE tabname,
         header  TYPE tt_table_header,
         content TYPE tt_table_content,
       END OF s_table,
       tt_table TYPE TABLE OF s_table WITH KEY name.

CONSTANTS: cv_export_content_none TYPE numc1 VALUE 1,
           cv_export_content_keys TYPE numc1 VALUE 2,
           cv_export_content_all  TYPE numc1 VALUE 3.

CONSTANTS: cv_import_content_reset  TYPE numc1 VALUE 1,
           cv_import_content_modify TYPE numc1 VALUE 2.

" Main Screen
DATA: p_value_file TYPE saepfad.

" Export Screen
DATA: p_table_name     TYPE tabname,
      p_trkorr         TYPE e070-trkorr,
      p_export_header  TYPE abap_bool,
      p_export_content TYPE char255.

" Import Screen
DATA: p_import_content TYPE char255,
      p_exported_file  TYPE saepfad.

DATA: gv_msg TYPE string.

DATA: gt_dynpfields        TYPE dynpread_tabtype,
      gt_dynpfields_export TYPE dynpread_tabtype,
      gt_dynpfields_import TYPE dynpread_tabtype.

DATA: gt_export_content TYPE vrm_values,
      gt_import_content TYPE vrm_values.

DATA: screen_id TYPE sy-dynnr,
      ok_code   LIKE sy-ucomm,
      save_ok   LIKE sy-ucomm,
      error     TYPE abap_bool.
