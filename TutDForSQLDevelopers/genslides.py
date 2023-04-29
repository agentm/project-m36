from glob import glob
#import imgkit
#from html2image import Html2Image

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
        htmldest = '{0}.html'.format(slidefile.split('.')[0])
        with open(htmldest, 'w') as proch:
            proch.write(htmlout)
        #convert to png
        imgout = '{0}.png'.format(slidefile.split('.')[0])
        #img = Html2Image(size=(1092,1080))
        #img.screenshot(html_file=htmldest,save_as=imgout)
        #imgkit.from_file(htmldest, imgout)
        print('Finished {0}'.format(imgout))
