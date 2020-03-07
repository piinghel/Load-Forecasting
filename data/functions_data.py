import urllib
import requests
from datetime import datetime as dt
import pandas as pd
import os


def save_in_dir(df, file_path, save_as):
    """

    :param df:
    :param file_path:
    :param save_as:
    :return:
    """
    directory = os.path.dirname(file_path)
    if not os.path.exists(directory):
        os.makedirs(directory)
    join_name_path = os.path.join(file_path, save_as)
    df.to_csv(path_or_buf=join_name_path, index=False)


def get_data_fifthplay(url='/api/v0.1/buildings/energyville1/fifthplay/',
                       user=None,
                       pswd=None,
                       DATE_FORMAT_STR="%Y-%m-%d",
                       start=dt(2017, 4, 27, 0, 0),
                       end=dt(2017, 9, 27, 0, 0),
                       device_ids='AE2648BF-173E-47ED-8E33-AA4C0EED1A8E',
                       columns=None,
                       directory=None,
                       save_as=None):
    """

    :param url:
    :param user:
    :param pswd:
    :param DATE_FORMAT_STR:
    :param start:
    :param end:
    :param device_ids:
    :param columns:
    :param directory:
    :param save_as:
    :return:
    """

    # url = urllib.parse.urljoin(URL, '/api/v0.1/buildings/energyville1/fifthplay/')
    url_parse = urllib.parse.urljoin('https://st-dev-data-api.azurewebsites.net', url)
    params = {'start': start.strftime(DATE_FORMAT_STR),
              'end': end.strftime(DATE_FORMAT_STR),
              'time_zone': 'Central European Standard Time',
              'device_ids': device_ids}

    result = requests.get(url_parse, params=params, auth=(user, pswd))
    try:
        df = pd.DataFrame(result.json()["data"])
        print(f"Successfull retrieved data")
        if url_parse[-8:-1] != "devices" and columns is not None:
            df = df[columns]
        if save_as is not None:
            if directory is not None:
                save_in_dir(df=df, file_path=directory, save_as=save_as)
            else:
                df.to_csv(path_or_buf=save_as, index=False)
            print(f'Successfully saved as {save_as} in {directory}')
        return df
    except ValueError:
        print('Conversion to pandas data frame went wrong')


def get_data_fluvius(url='/api/v0.1/buildings/energyville1/fluvius/',
                     user=None,
                     pswd=None,
                     DATE_FORMAT_STR="%m/%d/%Y %H:%M",
                     start=dt(2017, 4, 27, 0, 0),
                     end=dt(2017, 9, 27, 0, 0),
                     meter='541449200004157424',
                     columns=None,
                     directory=None,
                     save_as=None):
    """

    :param url:
    :param user:
    :param pswd:
    :param DATE_FORMAT_STR:
    :param start:
    :param end:
    :param meter:
    :param columns:
    :param directory:
    :param save_as:
    :return:
    """
    url_parse = urllib.parse.urljoin('https://st-dev-data-api.azurewebsites.net', url)
    params = {'start': start.strftime(DATE_FORMAT_STR),
              'end': end.strftime(DATE_FORMAT_STR),
              'time_zone': 'Central European Standard Time',
              'meter': meter}

    result = requests.get(url_parse, params=params, auth=(user, pswd))
    try:
        df = pd.DataFrame(result.json()["data"])
        if url_parse[-7:-1] != "meters" and columns is not None:
            df = df[columns]
        print(f'Successfull retrieved data')
        if save_as is not None:
            if directory is not None:
                save_in_dir(df=df, file_path=directory, save_as=save_as)
            else:
                df.to_csv(path_or_buf=save_as, index=False)
            print(f'Successfully saved as {save_as} in {directory}')
        return df
    except ValueError:
        print('Conversion to pandas data frame went wrong')
