from glob import glob

def process_template(template, template_variables, result_h):
    template.format(template_variables)


if __name__ == "__main__":
    #load template
    with open('template_tutd_sql.html','r') as template_h:
        template_tutd_sql = template_h.read()
    with open('template_tutd.html', 'r') as template_h:
        template_tutd = template_h.read()
    with open('template_sql.html', 'r') as template_h:
        template_sql = template_h.read()
    #identify all slides
    for slidefile in glob('*.slidedata'):
        with open(slidefile,'r') as slidef:
            slidedata = slidef.read()
        try:
            vals = slidedata.split('\n\n')
            if len(vals) == 6: #tutd + sql
                (title,tutd,tutd_res,sql,sql_res,english) = vals
                template = template_tutd_sql
            else:
                if 'SQL:' in vals[0]:
                    print('sql',vals[0])
                    (title,sql,sql_res,english) = vals
                    tutd = ''
                    tutd_res = ''
                    template = template_sql
                else:
                    (title,tutd,tutd_res,english) = vals
                    sql_res = ''
                    sql = ''
                    template = template_tutd
        except ValueError as err:
            print(f'Error in {slidefile}')
            raise err
        template_data = {'title':title,
                         'tutd':tutd,
                         'tutd_res':tutd_res,
                         'sql':sql,
                         'sql_res':sql_res,
                         'english':english}
        #process the template with data
        htmlout = template.format(**template_data)
        with open('{0}.html'.format(slidefile.split('.')[0]), 'w') as proch:
            proch.write(htmlout)
