*&---------------------------------------------------------------------*
*& Report zz_r_db_fill_by_excel
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zz_r_db_export_import.

"$. Region Declarations

CONSTANTS: cv_default_start_dir TYPE saepfad VALUE 'C:\',
           cv_default_separator TYPE c LENGTH 1 VALUE ';'.

DATA: p_table_name      TYPE dd02l-tabname,
      p_value_file      TYPE saepfad,
      p_separator       TYPE c LENGTH 1 VALUE cv_default_separator,
      p_export_only_hdr TYPE abap_bool.

DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm.

"$. Endregion Declarations

CALL SCREEN 100.

"$. Region Classes

CLASS lcl_db_export DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS: get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_db_export.
    METHODS execute.

  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO lcl_db_export.

    METHODS:
      convert_to_csv
        CHANGING ct_csv TYPE string_table,
      build_header_line
        CHANGING ct_csv TYPE string_table,
      download
        IMPORTING
          it_data TYPE STANDARD TABLE.

ENDCLASS.


CLASS lcl_db_export IMPLEMENTATION.

  METHOD get_instance.

    IF instance IS NOT BOUND.
      instance = NEW lcl_db_export( ).
    ENDIF.

    ro_instance = instance.

  ENDMETHOD.


  METHOD execute.

    DATA(lt_csv) = VALUE string_table( ).

    build_header_line(
      CHANGING
        ct_csv = lt_csv ).

    convert_to_csv(
      CHANGING
        ct_csv = lt_csv ).

    download( lt_csv ).

  ENDMETHOD.


  METHOD convert_to_csv.

    DATA: lv_csv           TYPE string,
          lv_field_content TYPE string,
          lr_table         TYPE REF TO data,
          lr_structure     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE data.

    CHECK p_export_only_hdr EQ abap_false.

    CREATE DATA lr_table TYPE STANDARD TABLE OF (p_table_name).
    ASSIGN lr_table->* TO <lt_table>.

    CREATE DATA lr_structure TYPE (p_table_name).
    ASSIGN lr_structure->* TO <ls_line>.

    SELECT FROM (p_table_name)
      FIELDS *
      INTO TABLE @<lt_table>.

    LOOP AT <lt_table> ASSIGNING <ls_line>.

      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_field>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        MOVE <lv_field> TO lv_field_content.
        CONDENSE lv_field_content.

        CASE sy-index.
          WHEN 1.
            lv_csv = lv_field_content.

          WHEN OTHERS.
            CONCATENATE lv_csv lv_field_content INTO lv_csv SEPARATED BY p_separator.

        ENDCASE.

      ENDDO.

      APPEND lv_csv TO ct_csv.

    ENDLOOP.

  ENDMETHOD.


  METHOD build_header_line.

    DATA(lv_csv) = VALUE string( ).

    SELECT FROM dd03l
      FIELDS *
      WHERE tabname  EQ @p_table_name
        AND comptype EQ 'E'
      ORDER BY position
      INTO TABLE @DATA(lt_dd03l).

    LOOP AT lt_dd03l ASSIGNING FIELD-SYMBOL(<ls_dd03l>).
      IF sy-tabix = 1.
        lv_csv = <ls_dd03l>-fieldname.
      ELSE.
        CONCATENATE lv_csv p_separator <ls_dd03l>-fieldname INTO lv_csv.
      ENDIF.
    ENDLOOP.

    APPEND lv_csv TO ct_csv.

  ENDMETHOD.


  METHOD download.

    DATA(lv_filename) = CONV string( p_value_file ).

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename = lv_filename
      TABLES
        data_tab = it_data.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_db_import DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS: get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_db_import.
    METHODS execute.

  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO lcl_db_import.



ENDCLASS.


CLASS lcl_db_import IMPLEMENTATION.

  METHOD get_instance.

    IF instance IS NOT BOUND.
      instance = NEW lcl_db_import( ).
    ENDIF.

    ro_instance = instance.

  ENDMETHOD.


  METHOD execute.

    DATA: lr_table     TYPE REF TO data,
          lr_structure TYPE REF TO data.

    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE,
                   <ls_line>  TYPE data.

    DATA(lt_data) = VALUE string_table( ).
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename = CONV #( p_value_file )
        filetype = 'ASC'
      CHANGING
        data_tab = lt_data ).

    CREATE DATA lr_table TYPE STANDARD TABLE OF (p_table_name).
    ASSIGN lr_table->* TO <lt_table>.

    CREATE DATA lr_structure TYPE (p_table_name).
    ASSIGN lr_structure->* TO <ls_line>.

    ##TODO " Import values by column name

    DATA(lt_header) = VALUE string_table( ).
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      SPLIT <ls_data> AT p_separator INTO TABLE DATA(lt_columns).

      IF lt_header IS INITIAL.

        lt_header = lt_columns.

      ELSE.

        LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<lv_header>).

          ASSIGN lt_columns[ sy-tabix ] TO FIELD-SYMBOL(<lv_value>).



        ENDLOOP.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.

"$. Endregion Classes

"$. Region Main

*&---------------------------------------------------------------------*
*&      Form  CHOOSE_VALUE_FILE
*&---------------------------------------------------------------------*
FORM choose_value_file.

  CONSTANTS: cv_default_ext TYPE string VALUE 'csv',
             cv_file_filter TYPE string VALUE '*.csv|*.csv|',
             cv_initial_dir TYPE string VALUE 'C:\'.

  DATA: lv_subrc      TYPE sy-subrc,
        lt_file_table TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = |Choose Excel file|
      default_extension       = cv_default_ext
      file_filter             = cv_file_filter
      initial_directory       = cv_initial_dir
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
    CLEAR: p_value_file.
  ELSE.
    p_value_file = <lv_file>.
  ENDIF.

ENDFORM.

FORM check_tabname CHANGING lv_is_valid.

  SELECT SINGLE
    FROM dd02l
    FIELDS @abap_true
    WHERE tabname  EQ @p_table_name
      AND tabclass EQ 'TRANSP'
      AND as4local EQ 'A'
    INTO @lv_is_valid.

  CHECK lv_is_valid EQ abap_false.

  MESSAGE |Table { p_table_name } doesn't exist.| TYPE 'S' DISPLAY LIKE 'E'.
  CLEAR: p_table_name.

ENDFORM.

FORM check_separator CHANGING lv_is_valid.

  IF p_separator EQ ''.
    p_separator = cv_default_separator.
    lv_is_valid = abap_false.
  ENDIF.

ENDFORM.

FORM read_values.

  DATA(lt_dynpfields) = VALUE dynpread_tabtype( ( fieldname = 'P_VALUE_FILE' )
                                                ( fieldname = 'DD02L-TABNAME' )
                                                ( fieldname = 'P_SEPARATOR' )
                                                ( fieldname = 'P_EXPORT_ONLY_HDR' ) ).

  CALL FUNCTION 'DYNP_VALUES_READ'
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
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  IF sy-subrc = 0.
    p_value_file      = VALUE #( lt_dynpfields[ fieldname = 'P_VALUE_FILE'      ]-fieldvalue OPTIONAL ).
    p_table_name      = VALUE #( lt_dynpfields[ fieldname = 'DD02L-TABNAME'     ]-fieldvalue OPTIONAL ).
    p_separator       = VALUE #( lt_dynpfields[ fieldname = 'P_SEPARATOR'       ]-fieldvalue OPTIONAL ).
    p_export_only_hdr = VALUE #( lt_dynpfields[ fieldname = 'P_EXPORT_ONLY_HDR' ]-fieldvalue OPTIONAL ).
  ENDIF.

ENDFORM.

FORM export.

  DATA(lv_is_valid) = abap_true.

  PERFORM check_tabname CHANGING lv_is_valid.
  PERFORM check_separator CHANGING lv_is_valid.
  CHECK lv_is_valid EQ abap_true.

  lcl_db_export=>get_instance( )->execute( ).

ENDFORM.

FORM import.

  DATA(lv_is_valid) = abap_true.

  PERFORM check_tabname CHANGING lv_is_valid.
  PERFORM check_separator CHANGING lv_is_valid.
  CHECK lv_is_valid EQ abap_true.

  lcl_db_import=>get_instance( )->execute( ).

ENDFORM.

"$. Endregion Main

"$. Region Modules

MODULE on_value_request_value_file INPUT.

  PERFORM choose_value_file.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  SET PF-STATUS 'MAIN100'.

ENDMODULE.                    "pbo_0100 OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  PERFORM read_values.

  CASE save_ok.
    WHEN 'EXPORT'.
      PERFORM export.

    WHEN 'IMPORT'.
      PERFORM import.

  ENDCASE.

ENDMODULE.                    "pai_0100 INPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE exit.

  save_ok = ok_code.
  CLEAR ok_code.

  LEAVE PROGRAM.

ENDMODULE.                    "exit

"$. Endregion Modules
