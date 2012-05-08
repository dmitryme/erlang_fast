-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).

-define(common_template_id_key, "201030081916789_common_template_id_key").

-define(template_dictionary(TemplateName), <<TemplateName/binary, <<"201030081916789_template_">>/binary >>).
-define(type_dictionary(Type), <<Type/binary, <<"201030081916789_type_">>/binary >>).