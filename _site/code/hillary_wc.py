import pandas as pd
import sqlite3

con = sqlite3.connect('../data/input/database.sqlite')
e = pd.read_sql_query("Select ExtractedBodyText From Emails where ExtractedBodyText like '%India%' limit 30",con)

cs = ""
for i in range(len(e.ExtractedBodyText)):
    cs += str(e.ExtractedBodyText[i].encode('utf-8').strip())

print cs

from wordcloud import WordCloud
from wordcloud import STOPWORDS
from os import path
from PIL import Image, ImageFile
import numpy as np
import matplotlib.pyplot as plt

ImageFile.LOAD_TRUNCATED_IMAGES = True
img = Image.open("hc.png")
img = img.resize((980,1080), Image.ANTIALIAS)
hcmask = np.array(img)


wc = WordCloud(background_color="white", max_words=2000, mask=hcmask, stopwords=STOPWORDS)
wc.generate(cs)
wc.to_file("../images/wc.png")
plt.imshow(wc)
plt.axis("off")
plt.figure()
plt.imshow(hcmask, cmap=plt.cm.gray)
plt.axis("off")
#plt.show()
#plt.savefig('../images/wc.png')


# combine with image of India dramatic effects
images = map(Image.open, ['india.jpg', '../images/wc.png'])

images[0] = images[0].resize((980,1080), Image.ANTIALIAS)
widths, heights = zip(*(i.size for i in images))

total_width = sum(widths)
max_height = max(heights)

new_im = Image.new('RGB', (total_width, max_height))

x_offset = 0
for im in images:
  new_im.paste(im, (x_offset,0))
  x_offset += im.size[0]

new_im.save('../images/final.jpg')

