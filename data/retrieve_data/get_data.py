import urllib
import requests
from datetime import datetime as dt
import pandas as pd
import os


def save_in_dir(df=None, file_path=None, file_name=None, verbose=True):
    """
    Saves a data frame

    :param df: Pandas Data Frame
        data frame to write to disk
    :param file_path: String
        location to write to disk
    :param file_name: String
        name of your file
    :param verbose: Boolean
        whether to print additional information when running (default is True)
    :return:
        None
    """
    directory = os.path.dirname(file_path)
    if not os.path.exists(directory):
        os.makedirs(directory)
    join_name_path = os.path.join(file_path, file_name)
    try:
        df.to_csv(path_or_buf=join_name_path, index=False)
        if verbose:
            print(f'Successfully saved as {file_name} in {file_path}')
    except ValueError:
        print(f'Could not save the file {file_name} in directory {file_path}')


def get_data_fifthplay(url='/api/v0.1/buildings/energyville1/fifthplay/',
                       user=None,
                       pswd=None,
                       date_format="%Y-%m-%d",
                       start=dt(2017, 4, 27, 0, 0),
                       end=dt(2017, 9, 27, 0, 0),
                       device_ids='AE2648BF-173E-47ED-8E33-AA4C0EED1A8E',
                       columns=None,
                       directory=None,
                       file_name=None,
                       verbose=True):
    """
    downloads data from the server and stores it in the given directory

    :param url: String
        url to retrieve the data
    :param user: String
        username
    :param pswd: String
        password to retrieve the data
    :param date_format: String

    :param start: Date
        start date
    :param end: Date
        end date
    :param device_ids: String
        device id to retrieve the data
    :param columns: List
        columns to retrieve
    :param directory: String
        location to write to disk
    :param file_name: String
        name of your file
    :param verbose: Boolean
        whether to print additional information when running (default is True)
    :return: Pandas Data Frame
        data
    """

    # url = urllib.parse.urljoin(URL, '/api/v0.1/buildings/energyville1/fifthplay/')
    url_parse = urllib.parse.urljoin('https://st-dev-data-api.azurewebsites.net', url)
    params = {'start': start.strftime(date_format),
              'end': end.strftime(date_format),
              'time_zone': 'Central European Standard Time',
              'device_ids': device_ids}

    result = requests.get(url_parse, params=params, auth=(user, pswd))
    try:
        df = pd.DataFrame(result.json()["data"])
        if verbose:
            print(f"Successful retrieved data")
        if url_parse[-8:-1] != "devices" and columns is not None:
            df = df[columns]
        if file_name is not None:
            if directory is not None:
                save_in_dir(df=df, file_path=directory, file_name=file_name, verbose=verbose)
            else:
                df.to_csv(path_or_buf=file_name, index=False)
                if verbose:
                    print(f'Successfully saved as {file_name} in {directory}')

        if verbose:
            print(f'Start date: {df.iloc[0,0]}')
            print(f'End date: {df.iloc[-1, 0]}')
        return df
    except ValueError:
        print('Conversion to pandas data frame went wrong!')


def get_data_fluvius(url='/api/v0.1/buildings/energyville1/fluvius/',
                     user=None,
                     pswd=None,
                     date_format="%m/%d/%Y %H:%M",
                     start=dt(2017, 4, 27, 0, 0),
                     end=dt(2017, 9, 27, 0, 0),
                     meter='541449200004157424',
                     columns=None,
                     directory=None,
                     file_name=None,
                     verbose=True):
    """
    downloads data from the server and stores it in the given directory

    :param url: String
        url to retrieve the data
    :param user: String
        username
    :param pswd: String
        password
    :param date_format: String
        format of the date
    :param start: Date
        start date
    :param end: Date
        end date
    :param meter: Integer
        meter id to retrieve the data
    :param columns: List
        columns to retrieve
    :param directory: String
        location to write to disk
    :param file_name: String
        name of your file
    :param verbose: Boolean
        whether to print additional information when running (default is True)
    :return: Pandas Data Frame
        data
    """
    url_parse = urllib.parse.urljoin('https://st-dev-data-api.azurewebsites.net', url)
    params = {'start': start.strftime(date_format),
              'end': end.strftime(date_format),
              'time_zone': 'Central European Standard Time',
              'meter': meter}

    result = requests.get(url_parse, params=params, auth=(user, pswd))
    try:
        df = pd.DataFrame(result.json()["data"])
        if url_parse[-7:-1] != "meters" and columns is not None:
            df = df[columns]
        if verbose:
            print(f'Successfull retrieved data')
        if file_name is not None:
            if directory is not None:
                save_in_dir(df=df, file_path=directory, file_name=file_name, verbose=verbose)
            else:
                df.to_csv(path_or_buf=file_name, index=False)
                if verbose:
                    print(f'Successfully saved as {file_name} in {directory}')
            if verbose:
                print(f'Start date: {df.iloc[0, 0]}')
                print(f'End date: {df.iloc[-1, 0]}')
        return df
    except ValueError:
        print('Conversion to pandas data frame went wrong!')
