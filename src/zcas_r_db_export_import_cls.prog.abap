*&---------------------------------------------------------------------*
*& Include zcas_r_db_export_import_cls
*&---------------------------------------------------------------------*
CLASS: lcl_db_table_export DEFINITION DEFERRED,
       lcl_db_table_import DEFINITION DEFERRED.

CLASS lcl_application DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_init,
      read_values,

      on_pbo_0100,
      on_pai_0100,
      on_exit,
      on_value_request_trkorr,
      on_value_request_value_file,
      on_value_request_exported_file,

      is_valid_file
        IMPORTING
          iv_file          TYPE saepfad
        RETURNING
          VALUE(rv_result) TYPE abap_bool,
      is_valid_table_name
        IMPORTING
          iv_table_name    TYPE tabname
        RETURNING
          VALUE(rv_result) TYPE abap_bool,
      serialize
        IMPORTING
          it_tables      TYPE tt_table
        RETURNING
          VALUE(rt_data) TYPE string_table,
      deserialize
        IMPORTING
          it_data          TYPE string_table
        RETURNING
          VALUE(rt_tables) TYPE tt_table,
      upload
        IMPORTING
          iv_file        TYPE saepfad
        RETURNING
          VALUE(rt_data) TYPE string_table,
      download
        IMPORTING
          iv_file TYPE saepfad
          it_data TYPE string_table.

  PROTECTED SECTION.
    CLASS-METHODS:
      open_table_sel,
      choose_file
        IMPORTING
          iv_filename TYPE string OPTIONAL
        CHANGING
          cv_file     TYPE saepfad,
      choose_tr.

ENDCLASS.


CLASS lcl_db_table_export DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      execute.

  PRIVATE SECTION.
    CLASS-METHODS:
      create_json
        RETURNING
          VALUE(rt_tables) TYPE tt_table,
      add_names_to_json
        RETURNING
          VALUE(rt_tables) TYPE tt_table,
      add_header_to_json
        CHANGING
          ct_tables TYPE tt_table,
      add_content_to_json
        CHANGING
          ct_tables TYPE tt_table,
      det_tables_by_name
        RETURNING
          VALUE(rt_tables) TYPE tt_table,
      det_tables_by_trkorr
        RETURNING
          VALUE(rt_tables) TYPE tt_table,
      det_tables_by_exported_file
        RETURNING
          VALUE(rt_tables) TYPE tt_table.

ENDCLASS.


CLASS lcl_db_table_import DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      execute.

  PRIVATE SECTION.
    CLASS-METHODS:
      change_db
        IMPORTING
          it_tables TYPE tt_table,
      add_dataset
        IMPORTING
          iv_table_name        TYPE tabname
          it_table_dataset     TYPE tt_table_dataset
        CHANGING
          cs_ddic_line_content TYPE data,
      import_content_modify
        IMPORTING
          iv_table_name    TYPE tabname
          it_table_content TYPE tt_table_content,
      import_content_reset
        IMPORTING
          iv_table_name    TYPE tabname
          it_table_content TYPE tt_table_content,
      find_origin_dataset
        IMPORTING
          it_ddic_table_content TYPE ANY TABLE
          it_ddic_table_attr    TYPE ddfields
          it_table_dataset      TYPE tt_table_dataset
        CHANGING
          cs_ddic_line_content  TYPE any.

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

  METHOD on_init.

    gt_dynpfields = VALUE #( ( fieldname = 'P_VALUE_FILE' ) ).

    gt_dynpfields_export = VALUE #( ( fieldname = 'P_TABLE_NAME' )
                                    ( fieldname = 'P_TRKORR' )
                                    ( fieldname = 'P_EXPORT_HEADER' )
                                    ( fieldname = 'P_EXPORT_CONTENT' )
                                    ( fieldname = 'P_EXPORTED_FILE' ) ).

    gt_dynpfields_import = VALUE #( ( fieldname = 'P_IMPORT_CONTENT' ) ).

    gt_export_content = VALUE #( ( key = cv_export_content_all  text = TEXT-001 )
                                 ( key = cv_export_content_keys text = TEXT-002 )
                                 ( key = cv_export_content_none text = TEXT-003 ) ).

    gt_import_content = VALUE #( ( key = cv_import_content_modify text = TEXT-004 )
                                 ( key = cv_import_content_reset  text = TEXT-005 ) ).

  ENDMETHOD.


  METHOD read_values.

    DO 2 TIMES.

      DATA(lv_index) = sy-index.

      CASE lv_index.
        WHEN 1.
          DATA(lv_dynnr) = '0100'.
          DATA(lt_dynpfields) = gt_dynpfields.

        WHEN 2.
          CASE main_tabstrip-activetab.
            WHEN 'TAB_EXPORT'.
              lv_dynnr = '0101'.
              lt_dynpfields = gt_dynpfields_export.

            WHEN 'TAB_IMPORT'.
              lv_dynnr = '0102'.
              lt_dynpfields = gt_dynpfields_import.

          ENDCASE.

      ENDCASE.

      CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
          dyname               = sy-repid
          dynumb               = lv_dynnr
        TABLES
          dynpfields           = lt_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          invalid_parameter    = 7
          undefind_error       = 8
          double_conversion    = 9
          stepl_not_found      = 10
          OTHERS               = 11.

      IF sy-subrc = 0.

        LOOP AT lt_dynpfields ASSIGNING FIELD-SYMBOL(<ls_dynpfield>).

          ASSIGN (<ls_dynpfield>-fieldname) TO FIELD-SYMBOL(<lv_parameter>).
          CHECK <lv_parameter> IS ASSIGNED.

          <lv_parameter> = <ls_dynpfield>-fieldvalue.

          UNASSIGN: <lv_parameter>.

        ENDLOOP.

      ELSE.

        MESSAGE TEXT-006 TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.

      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD is_valid_file.

    IF p_value_file CO ' _0'.
      rv_result = abap_false.
      MESSAGE TEXT-007 TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>file_exist(
      EXPORTING
        file                 = CONV #( iv_file )
      RECEIVING
        result               = rv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5 ).

    IF sy-subrc <> 0.
      rv_result = abap_false.
      gv_msg = TEXT-008.
      REPLACE '&1' IN gv_msg WITH iv_file.
      MESSAGE gv_msg TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_table_name.

    rv_result = abap_false.

    SELECT SINGLE
      FROM dd02l
      FIELDS @abap_true
      WHERE tabname  EQ @p_table_name
        AND tabclass EQ 'TRANSP'
        AND as4local EQ 'A'
      INTO @rv_result.

    CHECK rv_result EQ abap_false.

    error = abap_true.
    gv_msg = TEXT-009.
    REPLACE '&1' IN gv_msg WITH iv_table_name.
    MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDMETHOD.


  METHOD open_table_sel.

    CHECK is_valid_table_name( p_table_name ) EQ abap_true.

    CALL FUNCTION 'OM_FUNC_MODULE_EXIST'
      EXPORTING
        function_module = 'SE16N_INTERFACE'
      EXCEPTIONS
        OTHERS          = 99.

    IF sy-subrc <> 0.

      CALL FUNCTION 'RS_TABLE_LIST_CREATE'
        EXPORTING
          table_name = p_table_name
        EXCEPTIONS
          OTHERS     = 99.

    ELSE.

      CALL FUNCTION 'SE16N_INTERFACE'
        EXPORTING
          i_tab  = p_table_name
        EXCEPTIONS
          OTHERS = 99.

    ENDIF.

    IF sy-subrc <> 0.
      gv_msg = TEXT-010.
      REPLACE '&1' IN gv_msg WITH p_table_name.
      MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD choose_file.

    CONSTANTS: cv_default_ext TYPE string VALUE 'json',
               cv_file_filter TYPE string VALUE '*.json|*.json|',
               cv_initial_dir TYPE string VALUE '%USERPROFILE%\Downloads'.

    DATA: lv_subrc      TYPE sy-subrc,
          lt_file_table TYPE filetable.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = CONV #( TEXT-011 )
        default_extension       = cv_default_ext
        file_filter             = cv_file_filter
        initial_directory       = cv_initial_dir
        default_filename        = iv_filename
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_subrc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    ASSIGN lt_file_table[ 1 ] TO FIELD-SYMBOL(<lv_file>).
    IF <lv_file> IS NOT ASSIGNED.
      CLEAR: cv_file.
    ELSE.
      cv_file = <lv_file>.
    ENDIF.

  ENDMETHOD.


  METHOD choose_tr.

    DATA: lt_results TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

    SELECT FROM e070
      FIELDS *
      WHERE trstatus EQ @sctsc_state_changeable
        AND as4user  EQ @sy-uname
        AND strkorr  EQ @space
      INTO TABLE @DATA(lt_help_values).

    DATA(lv_window_title) = CONV string( TEXT-012 ).
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure = 'E070'
        retfield       = 'TRKORR'
        dynpprog       = sy-repid
        dynpnr         = sy-dynnr
        window_title   = lv_window_title
        value_org      = 'S'
      TABLES
        value_tab      = lt_help_values
        return_tab     = lt_results
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      MESSAGE TEXT-013 TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

    DATA(lt_dynpfields) = VALUE dynpread_tabtype( ( fieldname = 'P_TRKORR'
                                                    fieldvalue = VALUE #( lt_results[ 1 ]-fieldval OPTIONAL ) ) ).

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.

  ENDMETHOD.


  METHOD upload.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = CONV #( iv_file )
        filetype                = 'ASC'
      CHANGING
        data_tab                = rt_data
      EXCEPTIONS
        OTHERS                  = 99 ).

    IF sy-subrc <> 0.
      error = abap_true.
      MESSAGE TEXT-014 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD download.

    DATA(lt_data) = it_data.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename                  = CONV string( iv_file )
      CHANGING
        data_tab                  = lt_data
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).

    IF sy-subrc <> 0.
      error = abap_true.
      MESSAGE TEXT-015 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD serialize.

    DATA(lv_json) = /ui2/cl_json=>serialize( data = it_tables ).
    APPEND lv_json TO rt_data.

  ENDMETHOD.


  METHOD deserialize.

    DATA(lv_json) = VALUE string( ).
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<lv_data>).
      lv_json = |{ lv_json }{ <lv_data> }|.
    ENDLOOP.

    /ui2/cl_json=>deserialize( EXPORTING json = lv_json
                               CHANGING  data = rt_tables ).

  ENDMETHOD.


  METHOD on_pbo_0100.

    SET PF-STATUS 'MAIN100'.

    CASE main_tabstrip-activetab.
      WHEN 'TAB_EXPORT'.
        screen_id = '101'.

      WHEN 'TAB_IMPORT'.
        screen_id = '102'.

      WHEN OTHERS.
        screen_id = '101'.

    ENDCASE.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_EXPORT_CONTENT'
        values          = gt_export_content
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

    IF p_export_content CO ' _0'.
      p_export_content = cv_export_content_all.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_IMPORT_CONTENT'
        values          = gt_import_content
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.

    IF p_import_content CO ' _0'.
      p_import_content = cv_import_content_modify.
    ENDIF.

  ENDMETHOD.


  METHOD on_pai_0100.

    error = abap_false.
    save_ok = ok_code.
    CLEAR ok_code.

    lcl_application=>read_values( ).

    CASE save_ok.
      WHEN 'TAB_EXPORT' OR 'TAB_IMPORT'.
        main_tabstrip-activetab = save_ok.
        CLEAR: p_value_file.

      WHEN 'EXECUTE'.
        CASE main_tabstrip-activetab.
          WHEN 'TAB_EXPORT'.
            lcl_db_table_export=>execute( ).

          WHEN 'TAB_IMPORT'.
            lcl_db_table_import=>execute( ).

        ENDCASE.

      WHEN 'OPEN_SE16N'.
        lcl_application=>open_table_sel( ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_exit.

    save_ok = ok_code.
    CLEAR ok_code.

    LEAVE PROGRAM.

  ENDMETHOD.


  METHOD on_value_request_trkorr.

    lcl_application=>choose_tr( ).

  ENDMETHOD.


  METHOD on_value_request_value_file.

    IF main_tabstrip-activetab EQ 'TAB_EXPORT'.
      DATA(lv_filename) = |{ sy-sysid }_db_export_{ sy-datum }_{ sy-uzeit }|.
    ENDIF.

    lcl_application=>choose_file( EXPORTING iv_filename = lv_filename
                                  CHANGING  cv_file     = p_value_file ).

  ENDMETHOD.


  METHOD on_value_request_exported_file.

    lcl_application=>choose_file( CHANGING cv_file = p_exported_file ).

  ENDMETHOD.

ENDCLASS.


CLASS lcl_db_table_export IMPLEMENTATION.

  METHOD execute.

    CHECK lcl_application=>is_valid_file( p_value_file ) EQ abap_true.

    WHILE error EQ abap_false.

      DATA(lv_index) = sy-index.

      CASE lv_index.
        WHEN 1.
          DATA(lt_tables) = create_json( ).

        WHEN 2.
          DATA(lt_data) = lcl_application=>serialize( lt_tables ).

        WHEN 3.
          lcl_application=>download( iv_file = p_value_file
                                     it_data = lt_data ).

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.


  METHOD create_json.

    WHILE error EQ abap_false.

      DATA(lv_index) = sy-index.

      CASE lv_index.
        WHEN 1.
          rt_tables = add_names_to_json( ).

        WHEN 2.
          IF p_export_header EQ abap_true.
            add_header_to_json( CHANGING  ct_tables = rt_tables ).
          ENDIF.

        WHEN 3.
          IF p_export_content NE cv_export_content_none.
            add_content_to_json( CHANGING ct_tables = rt_tables ).
          ENDIF.

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.


  METHOD add_names_to_json.

    IF p_table_name CN ' _0'.

      rt_tables = det_tables_by_name( ).

    ELSEIF p_trkorr CN ' _0'.

      rt_tables = det_tables_by_trkorr( ).

    ELSEIF p_exported_file CN ' _0'.

      rt_tables = det_tables_by_exported_file( ).

    ELSE.

      error = abap_true.
      MESSAGE TEXT-016 TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD add_header_to_json.

    LOOP AT ct_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      DATA: lo_struct_descr TYPE REF TO cl_abap_structdescr.

      lo_struct_descr ?= cl_abap_structdescr=>describe_by_name( <ls_table>-name ).
      DATA(lt_ddic_table_attr) = lo_struct_descr->get_ddic_field_list( p_including_substructres = abap_true ).
      SORT lt_ddic_table_attr BY position.

      <ls_table>-header = VALUE tt_table_header( FOR <s_ddic_table_attr> IN lt_ddic_table_attr
                                                   ( fieldname = <s_ddic_table_attr>-fieldname ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_content_to_json.

    DATA: lv_field_value  TYPE string,
          lo_table        TYPE REF TO data,
          lo_structure    TYPE REF TO data,
          lo_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <lt_table_content> TYPE STANDARD TABLE,
                   <ls_line_content>  TYPE data.

    LOOP AT ct_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      DATA(lt_table_content) = VALUE tt_table_content( ).

      CREATE DATA lo_table TYPE STANDARD TABLE OF (<ls_table>-name).
      ASSIGN lo_table->* TO <lt_table_content>.

      CREATE DATA lo_structure TYPE (<ls_table>-name).
      ASSIGN lo_structure->* TO <ls_line_content>.

      lo_struct_descr ?= cl_abap_typedescr=>describe_by_data( <ls_line_content> ).
      DATA(lt_ddic_table_attr) = lo_struct_descr->get_ddic_field_list( p_including_substructres = abap_true ).
      SORT lt_ddic_table_attr BY position.

      SELECT FROM (<ls_table>-name)
        FIELDS *
        INTO TABLE @<lt_table_content>.

      " Iterate over rows
      LOOP AT <lt_table_content> ASSIGNING <ls_line_content>.

        APPEND INITIAL LINE TO lt_table_content ASSIGNING FIELD-SYMBOL(<ls_table_content>).

        " Iterate over columns
        LOOP AT lt_ddic_table_attr ASSIGNING FIELD-SYMBOL(<ls_ddic_table_attr>).

          IF     p_export_content             EQ cv_export_content_keys
             AND <ls_ddic_table_attr>-keyflag EQ abap_false.
            EXIT.
          ENDIF.

          ASSIGN COMPONENT <ls_ddic_table_attr>-fieldname OF STRUCTURE <ls_line_content> TO FIELD-SYMBOL(<lv_field_value>).

          MOVE <lv_field_value> TO lv_field_value.
          CONDENSE lv_field_value.

          DATA(ls_table_dataset) = VALUE s_table_dataset( param  = <ls_ddic_table_attr>-fieldname
                                                          value  = lv_field_value
                                                          is_key = <ls_ddic_table_attr>-keyflag ).
          APPEND ls_table_dataset TO <ls_table_content>-dataset.

          UNASSIGN: <lv_field_value>.

        ENDLOOP.

      ENDLOOP.

      IF lt_table_content IS NOT INITIAL.
        <ls_table>-content = lt_table_content.
      ENDIF.

      CLEAR: lo_table, lo_structure, lo_struct_descr, lt_ddic_table_attr, lt_table_content.
      UNASSIGN: <lt_table_content>.

    ENDLOOP.

  ENDMETHOD.


  METHOD det_tables_by_name.

    CHECK lcl_application=>is_valid_table_name( p_table_name ) EQ abap_true.
    rt_tables = VALUE #( ( name = |{ p_table_name }| ) ).

  ENDMETHOD.


  METHOD det_tables_by_trkorr.

    DATA(lt_table_keys) = VALUE tr_keys( ).
    DATA(ls_request_header) = VALUE trwbo_request_header( trkorr = p_trkorr ).

    CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
      EXPORTING
        is_request_header = ls_request_header
      IMPORTING
        et_keys           = lt_table_keys
      EXCEPTIONS
        invalid_input     = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      error = abap_true.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
      RETURN.
    ENDIF.

    DATA(lt_table_names) = VALUE string_table( FOR <s_table_key> IN lt_table_keys
                                                 ( CONV #( <s_table_key>-objname ) ) ).
    SORT lt_table_names.
    DELETE ADJACENT DUPLICATES FROM lt_table_names.

    rt_tables = VALUE #( FOR <v_table_name> IN lt_table_names
                           ( name = <v_table_name> ) ).

  ENDMETHOD.


  METHOD det_tables_by_exported_file.

    CHECK lcl_application=>is_valid_file( p_exported_file ) EQ abap_true.

    WHILE error EQ abap_false.

      DATA(lv_index) = sy-index.

      CASE lv_index.
        WHEN 1.
          DATA(lt_data) = lcl_application=>upload( p_exported_file ).

        WHEN 2.
          rt_tables = lcl_application=>deserialize( lt_data ).

          LOOP AT rt_tables ASSIGNING FIELD-SYMBOL(<ls_table>).
            CLEAR: <ls_table>-content, <ls_table>-header.
          ENDLOOP.

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_db_table_import IMPLEMENTATION.

  METHOD execute.

    CHECK lcl_application=>is_valid_file( p_value_file ) EQ abap_true.

    WHILE error EQ abap_false.

      DATA(lv_index) = sy-index.

      CASE lv_index.
        WHEN 1.
          DATA(lt_data) = lcl_application=>upload( p_value_file ).

        WHEN 2.
          DATA(lt_tables) = lcl_application=>deserialize( lt_data ).

        WHEN 3.
          change_db( lt_tables ).

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.


  METHOD change_db.

    LOOP AT it_tables ASSIGNING FIELD-SYMBOL(<ls_table>).

      CASE p_import_content.
        WHEN cv_import_content_modify.
          import_content_modify( iv_table_name    = <ls_table>-name
                                 it_table_content = <ls_table>-content ).

        WHEN cv_import_content_reset.
          import_content_reset( iv_table_name    = <ls_table>-name
                                it_table_content = <ls_table>-content ).

      ENDCASE.

    ENDLOOP.

    COMMIT WORK.

    CASE sy-subrc.
      WHEN 0.
        MESSAGE TEXT-017 TYPE 'S'.

    WHEN OTHERS.
      gv_msg = TEXT-018.
      REPLACE '&1' IN gv_msg WITH CONV string( sy-subrc ).
        MESSAGE gv_msg TYPE 'S' DISPLAY LIKE 'E'.

    ENDCASE.

  ENDMETHOD.


  METHOD add_dataset.

    IF cs_ddic_line_content IS NOT INITIAL.
      DATA(lv_modify_existent) = abap_true.
    ENDIF.

    LOOP AT it_table_dataset ASSIGNING FIELD-SYMBOL(<ls_table_dataset>).

      IF lv_modify_existent EQ abap_true.
        CHECK <ls_table_dataset>-is_key EQ abap_false.
      ENDIF.

      ASSIGN COMPONENT <ls_table_dataset>-param OF STRUCTURE cs_ddic_line_content TO FIELD-SYMBOL(<lv_value>).
      IF <ls_table_dataset>-param EQ 'MANDT'.
        <lv_value> = sy-mandt.
      ELSE.
        <lv_value> = <ls_table_dataset>-value.
      ENDIF.

      UNASSIGN: <lv_value>.

    ENDLOOP.

    MODIFY (iv_table_name) FROM cs_ddic_line_content.

  ENDMETHOD.


  METHOD import_content_modify.

    DATA: lo_table        TYPE REF TO data,
          lo_structure    TYPE REF TO data,
          lo_struct_descr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_ddic_line_content>  TYPE any,
                   <lt_ddic_table_content> TYPE STANDARD TABLE.

    CREATE DATA lo_table TYPE STANDARD TABLE OF (iv_table_name).
    ASSIGN lo_table->* TO <lt_ddic_table_content>.

    CREATE DATA lo_structure TYPE (iv_table_name).
    ASSIGN lo_structure->* TO <ls_ddic_line_content>.

    lo_struct_descr ?= cl_abap_typedescr=>describe_by_name( iv_table_name ).
    DATA(lt_ddic_table_attr) = lo_struct_descr->get_ddic_field_list( p_including_substructres = abap_true ).
    SORT lt_ddic_table_attr BY position.

    SELECT FROM (iv_table_name)
      FIELDS *
      INTO TABLE @<lt_ddic_table_content>.

    LOOP AT it_table_content ASSIGNING FIELD-SYMBOL(<ls_table_content>).

      CLEAR: <ls_ddic_line_content>.

      IF line_exists( lt_ddic_table_attr[ keyflag = abap_false ] ).

        find_origin_dataset( EXPORTING it_ddic_table_content = <lt_ddic_table_content>
                                       it_ddic_table_attr    = lt_ddic_table_attr
                                       it_table_dataset      = <ls_table_content>-dataset
                             CHANGING  cs_ddic_line_content  = <ls_ddic_line_content> ).

        add_dataset( EXPORTING iv_table_name        = iv_table_name
                               it_table_dataset     = <ls_table_content>-dataset
                     CHANGING  cs_ddic_line_content = <ls_ddic_line_content> ).

      ELSE.

        " Add dataset if not existent and only consistent of primary keys
        add_dataset( EXPORTING iv_table_name        = iv_table_name
                               it_table_dataset     = <ls_table_content>-dataset
                     CHANGING  cs_ddic_line_content = <ls_ddic_line_content> ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD import_content_reset.

    DATA: lo_structure TYPE REF TO data.

    FIELD-SYMBOLS: <ls_ddic_line_content> TYPE any.

    CREATE DATA lo_structure TYPE (iv_table_name).
    ASSIGN lo_structure->* TO <ls_ddic_line_content>.

    DELETE FROM (iv_table_name).

    LOOP AT it_table_content ASSIGNING FIELD-SYMBOL(<ls_table_content>).

      add_dataset( EXPORTING iv_table_name         = iv_table_name
                             it_table_dataset      = <ls_table_content>-dataset
                   CHANGING  cs_ddic_line_content  = <ls_ddic_line_content> ).

      CLEAR: <ls_ddic_line_content>.

    ENDLOOP.

  ENDMETHOD.


  METHOD find_origin_dataset.

    DATA(lv_found_existing) = abap_false.

    " Iterate over import and origin rows: Find origin dataset by primary key and change related dataset
    LOOP AT it_ddic_table_content ASSIGNING FIELD-SYMBOL(<ls_origin_content>).

      LOOP AT it_ddic_table_attr ASSIGNING FIELD-SYMBOL(<ls_ddic_table_attr>) WHERE keyflag   EQ abap_true
                                                                                AND fieldname NE 'MANDT'.

        ASSIGN it_table_dataset[ param = <ls_ddic_table_attr>-fieldname ] TO FIELD-SYMBOL(<ls_table_attr>).
        IF <ls_table_attr> IS NOT ASSIGNED.
          " Incomplete export of primary keys: Ignore dataset
          error = abap_true.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT <ls_ddic_table_attr>-fieldname OF STRUCTURE <ls_origin_content> TO FIELD-SYMBOL(<lv_origin_value>).

        IF <ls_table_attr>-value EQ <lv_origin_value>.
          lv_found_existing = abap_true.
        ELSE.
          lv_found_existing = abap_false.
          EXIT.
        ENDIF.

        UNASSIGN: <ls_table_attr>, <lv_origin_value>.

      ENDLOOP.

      IF   error             EQ abap_true
        OR lv_found_existing EQ abap_true.
        cs_ddic_line_content = <ls_origin_content>.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
