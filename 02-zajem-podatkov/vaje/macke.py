import csv
import os
import requests
import re

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definirajte URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = "cats"
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = "index.html"
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = "data.csv"

# TODO: finish urltostring
# TODO save_string_to_file v save_frontpage
# TODO: save_frontpage and check it really gives the html

def download_url_to_string(url):
    """Funkcija kot argument sprejme niz in poskusi vrniti vsebino te spletne
    strani kot niz. V primeru, da med izvajanje pride do napake vrne None.
    """
    try:
        # del kode, ki morda sproži napako
        page_content = requests.get(url)
    except Exception as e:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print("Prišlo je do napake pri povezovanju")
        print(e)

        return None
    # nadaljujemo s kodo če ni prišlo do napake
    if page_content.status_code == requests.codes.ok:
        return page_content.text
    else:
        print("Težava pri vsebini strani")
        return None


def save_string_to_file(text, directory, filename):
    """Funkcija zapiše vrednost parametra "text" v novo ustvarjeno datoteko
    locirano v "directory"/"filename", ali povozi obstoječo. V primeru, da je
    niz "directory" prazen datoteko ustvari v trenutni mapi.
    """
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None


# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage(page, directory, filename):
    """Funkcija shrani vsebino spletne strani na naslovu "page" v datoteko
    "directory"/"filename"."""
    html = download_url_to_string(page)
    if html: 
        save_string_to_file(html, directory, filename)
        return True
    else:
        return False


###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
