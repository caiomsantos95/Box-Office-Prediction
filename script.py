from bs4 import BeautifulSoup
import pandas as pd
import requests
import time
import regex as re
from numpy import random

url = ('http://www.imdb.com/search/title?count=200&view=simple'
    '&boxoffice_gross_us=1,&title_type=feature&release_date={year}')

headers = {
    'Accept-Language': 'en-US',
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    }

def get_movies(year):
    '''Get list of movies released in <year>.'''
    movie_list = []
    next_url = url.format(year=year)
    while(next_url is not None):
        movies_html = requests.get(next_url, headers=headers).text
        soup = BeautifulSoup(movies_html, 'html.parser')
        movies = soup.find_all('a', href=re.compile('adv_li_tt'))
        movie_list+=['http://www.imdb.com' + m['href'] for m in movies]
        try:
            next_url = 'http://www.imdb.com' + soup.find('a', href=re.compile('/search/title'), text=re.compile('Next'))['href']
        except TypeError:
            next_url = None

    return movie_list


def go_to_movie(url):
    '''Get IMDb page of a movie.'''
    movie_html = requests.get(url, headers=headers).text

    return movie_html


def get_money(soup, type):
    try:
        wrapper = soup.find('span', text=type).findNext('div')
        money = wrapper.find('span').text
        
        return money
    except AttributeError:
        return None


def get_company(soup):
    wrapper = soup.find('a', text='Production companies')

    if not wrapper:
        wrapper = soup.find('a', text='Production company')

    try:
        company = wrapper.findNext('div').find('a').text
    except AttributeError:
        return None

    return company


def get_release_date(soup):
    try:
        wrapper = soup.find('a', text='Release date').findNext('div')
        release_date = wrapper.find('a').text

        return release_date
    except AttributeError:
        return None


def get_runtime(soup):
    try:
        wrapper = soup.find('span', text='Runtime').findNext('div')
        runtime = wrapper.find('span').text.split()
    except AttributeError:
        return None
    if len(runtime) > 1:
        hours = int(runtime[0].replace('h', '')) * 60
        minutes = int(runtime[1].replace('min', ''))

        return hours + minutes
    elif 'h' in runtime[0]:
        return int(runtime[0].replace('h', '')) * 60
    elif 'min' in runtime[0]:
        return int(runtime[0].replace('min', ''))


def get_star(soup):

    wrapper = soup.find('a', text='Stars')
    if not wrapper:
        wrapper = soup.find('span', text='Stars')
        
    try:
        wrapper = wrapper.findNext('div')
    except AttributeError:
        return None
    
    try:
        return wrapper.find('a').text
    except AttributeError:
        return None
        

def get_writer(soup):
    try:
        writer = soup.find('a', {'href': re.compile('tt_ov_wr')}).text
    except AttributeError:
        return None
    
    if writer == 'Writers':
        try:
            wrapper = soup.find('a', text='Writers').findNext('div')
        except AttributeError:
            wrapper = soup.find('span', text='Writers').findNext('div') 
        writer = wrapper.find('a').text

    return writer


def get_country(soup):
    wrapper = soup.find('span', text='Country of origin')
    if not wrapper:
        wrapper = soup.find('span', text='Countries of origin')

    try:
        wrapper = wrapper.findNext('div')
    except AttributeError:
        return None

    try:
        return wrapper.find('a').text
    except AttributeError:
        return None



def scrap_titlebar(soup, year):
    '''Get name, rating, genre, year, release date, score and votes of a movie.'''
    released = get_release_date(soup)

    try:
        rating = soup.find('a', {'href': re.compile('tt_ov_pg')}).text
    except AttributeError:
        rating = None

    titlebar = {
        'year': year,
        'released': released,
        'rating': rating
    }

    return titlebar

def scrap_crew(soup):
    '''Get director, writer and star of a movie.'''
    try:
        director = soup.find('a', {'href': re.compile('tt_ov_dr')}).text
    except AttributeError:
        director = None
    writer = get_writer(soup)
    star = get_star(soup)

    crew = {
        'director': director,
        'writer': writer,
        'star': star
    }

    return crew


def scrap_details(soup):
    '''Get country, budget, gross, production co. and runtime of a movie.'''
    country = get_country(soup)
    gross = get_money(soup, type='Gross worldwide')
    budget = get_money(soup, type='Budget')
    company = get_company(soup)
    runtime = get_runtime(soup)

    budget_currency = None
    budget_value = None

    if budget:
        try:
            budget_re = re.search("(\D*)(.*)",budget)
            budget_currency = budget_re.group(1)
            budget_value = float(budget_re.group(2).split()[0].replace(',',''))
        except:
            budget_currency = None
            budget_value = None

    gross_currency = None
    gross_value = None


    if gross:
        try:
            gross_re = re.search("(\D*)(.*)",gross)
            gross_currency = gross_re.group(1)
            gross_value = float(gross_re.group(2).replace(',',''))
        except:
            gross_currency = None
            gross_value = None

    details = {
        'country': country,
        'budget': budget,
        'budget currency': budget_currency,
        'budget value': budget_value,
        'gross': gross,
        'gross currency': gross_currency,
        'gross value': gross_value,
        'company': company,
        'runtime': runtime
    }

    return details


def write_csv(data):
    '''Write list of dicts to csv.'''
    df = pd.DataFrame(data)
    df.to_csv('movies_2000_2005.csv', index=False)


def main():
    all_movie_data = []
    for year in range(2017, 2018):
        movies = get_movies(year)
        for movie_url in movies:
            movie_data = {}
            movie_html = go_to_movie(movie_url)
            soup = BeautifulSoup(movie_html, 'html.parser')
            movie_data.update({'movie_url': movie_url})
            movie_id=re.search("(?:title\/)(tt\d*)","https://www.imdb.com/title/tt6182908/?ref_=adv_li_tt").group(1)
            movie_data.update({'movie_id': movie_id})
            movie_data.update(scrap_titlebar(soup, year))
            movie_data.update(scrap_crew(soup))
            movie_data.update(scrap_details(soup))
            all_movie_data.append(movie_data)
            time.sleep(random.uniform(0.1,0.5))
            print(movie_url,'done.')
        print(year, 'done.')

    write_csv(all_movie_data)


if __name__ == '__main__':
    main()