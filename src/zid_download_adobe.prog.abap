*-------------------------------------------------------------------------------*
*MIT License
*
*Copyright (c) 2019 Jacek Kopcinski (indevo.pl)
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
*-------------------------------------------------------------------------------*

REPORT  zid_download_adobe.

TABLES:
  sfpverscform.

TYPES:
  t_forms TYPE STANDARD TABLE OF fpname,
  t_interfaces TYPE STANDARD TABLE OF fpname.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
SELECT-OPTIONS:
  s_form FOR sfpverscform-name,
  s_intf FOR sfpverscform-interface.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
PARAMETERS:
  p_downld TYPE abap_bool RADIOBUTTON GROUP rb1,
  p_delete TYPE abap_bool RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b02.

START-OF-SELECTION.
  PERFORM download.

FORM select_forms
  CHANGING c_forms TYPE t_forms.
  CHECK s_form IS NOT INITIAL.
  SELECT name FROM fpcontext INTO TABLE c_forms
    WHERE name IN s_form.
ENDFORM.

FORM select_interfaces
  CHANGING c_interfaces TYPE t_interfaces.
  CHECK s_intf IS NOT INITIAL.
  SELECT name FROM fpinterface INTO TABLE c_interfaces
    WHERE name IN s_intf.
ENDFORM.

FORM download.

  TRY .
      PERFORM process_forms.
      PERFORM process_interfaces.

    CATCH cx_fp_ui_workbench.
      MESSAGE 'Action cancelled' TYPE 'S' DISPLAY LIKE 'W'.
  ENDTRY.

ENDFORM.

FORM process_forms
RAISING cx_fp_ui_workbench.

  DATA:
    forms TYPE t_forms,
    form_name TYPE fpname,
    wb_form TYPE REF TO if_fp_wb_form.

  PERFORM select_forms CHANGING forms.
  LOOP AT forms INTO form_name.
    IF p_downld = abap_true.
      wb_form = cl_fp_wb_form=>load( form_name ).
      PERFORM download_form USING wb_form.
    ELSE.
      cl_fp_wb_form=>delete( form_name ).
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM process_interfaces
RAISING cx_fp_ui_workbench.

  DATA:
    interfaces TYPE t_interfaces,
    interface_name TYPE fpname,
    wb_interface TYPE REF TO if_fp_wb_interface.

  PERFORM select_interfaces CHANGING interfaces.
  LOOP AT interfaces INTO interface_name.
    IF p_downld = abap_true.
      wb_interface = cl_fp_wb_interface=>load( interface_name ).
      PERFORM download_interface USING wb_interface.
    ELSE.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM download_form
  USING
  i_wb_form TYPE REF TO if_fp_wb_form
  RAISING cx_fp_ui_workbench.


  TYPES: t_raw(1024) TYPE x.

  DATA:
        l_form           TYPE REF TO   if_fp_form,
        l_name           TYPE          string,
        l_file_table     TYPE          filetable,
        l_filename       TYPE          string,
        l_pathname       TYPE          string,
        l_fullpath       TYPE          string,
        l_rc             TYPE          i,
        l_user_action    TYPE          i,
        l_xstring        TYPE          xstring,
        l_binary_table   TYPE TABLE OF t_raw,
        l_binary_length  TYPE          i,
        l_node           TYPE REF TO   if_fp_node.

  l_form ?= i_wb_form->get_object( ).
  l_name = i_wb_form->get_name( ).

  CONCATENATE 'SFPF_' l_name INTO l_name.

  REPLACE ALL OCCURRENCES OF '/' IN l_name WITH '_'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = cl_gui_frontend_services=>filetype_xml
      default_file_name    = l_name
      file_filter          = cl_gui_frontend_services=>filetype_xml
    CHANGING
      filename             = l_filename
      path                 = l_pathname
      fullpath             = l_fullpath
      user_action          = l_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc = 0 AND l_user_action <> cl_gui_frontend_services=>action_cancel.
    TRY.
        l_xstring = cl_fp_helper=>convert_form_to_xstring( l_form ).
      CATCH cx_fp_api_internal.
        RAISE EXCEPTION TYPE cx_fp_ui_workbench.
    ENDTRY.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = l_xstring
      IMPORTING
        output_length = l_binary_length
      TABLES
        binary_tab    = l_binary_table.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = l_binary_length
        filename     = l_fullpath
        filetype     = 'BIN'
      TABLES
        data_tab     = l_binary_table
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_fp_ui_workbench.
    ENDIF.

    MESSAGE s070(fpuifb).
  ELSE.
    RAISE EXCEPTION TYPE cx_fp_ui_workbench
      EXPORTING
        textid = cx_fp_ui_workbench=>action_cancelled.
  ENDIF.

ENDFORM.

FORM download_interface
  USING i_wb_interface TYPE REF TO if_fp_wb_interface
  RAISING cx_fp_ui_workbench.

  TYPES: t_raw(1024) TYPE x.

  DATA:
        l_interface      TYPE REF TO   if_fp_interface,
        l_name           TYPE          string,
        l_file_table     TYPE          filetable,
        l_filename       TYPE          string,
        l_pathname       TYPE          string,
        l_fullpath       TYPE          string,
        l_rc             TYPE          i,
        l_user_action    TYPE          i,
        l_xstring        TYPE          xstring,
        l_binary_table   TYPE TABLE OF t_raw,
        l_binary_length  TYPE          i,
        l_node           TYPE REF TO   if_fp_node.

  l_interface ?= i_wb_interface->get_object( ).
  l_name = i_wb_interface->get_name( ).

  CONCATENATE 'SFPI_' l_name INTO l_name.

  REPLACE ALL OCCURRENCES OF '/' IN l_name WITH '_'.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension    = cl_gui_frontend_services=>filetype_xml
      default_file_name    = l_name
      file_filter          = cl_gui_frontend_services=>filetype_xml
    CHANGING
      filename             = l_filename
      path                 = l_pathname
      fullpath             = l_fullpath
      user_action          = l_user_action
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc = 0 AND l_user_action <> cl_gui_frontend_services=>action_cancel.
    TRY.
        l_xstring = cl_fp_helper=>convert_interface_to_xstring( l_interface ).
      CATCH cx_fp_api_internal.
        RAISE EXCEPTION TYPE cx_fp_ui_workbench.
    ENDTRY.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = l_xstring
      IMPORTING
        output_length = l_binary_length
      TABLES
        binary_tab    = l_binary_table.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = l_binary_length
        filename     = l_fullpath
        filetype     = 'BIN'
      TABLES
        data_tab     = l_binary_table
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_fp_ui_workbench.
    ENDIF.

    MESSAGE s072(fpuifb).
  ELSE.
    RAISE EXCEPTION TYPE cx_fp_ui_workbench
      EXPORTING
        textid = cx_fp_ui_workbench=>action_cancelled.
  ENDIF.

ENDFORM.
