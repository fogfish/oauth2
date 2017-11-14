import React from 'react';
import ReactDOM from 'react-dom';
import OAuth2 from './app';

import {Provider} from 'react-redux';
import {store} from './store';


ReactDOM.render(
   <Provider store={store}>
      <OAuth2 />
   </Provider>,
   document.getElementById('root')
);

