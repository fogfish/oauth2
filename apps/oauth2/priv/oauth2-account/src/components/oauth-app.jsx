import React from 'react';
import {Window, Dialog, DialogWithOk, DialogWithAccept} from './dress-code';


const AppName = () => (
   <div>
      <label className="dc-label">
         Name <span className="dc-label__sub">required</span>
      </label>
      <input type="input" className="dc-input" autoFocus="true" required="required" />   
   </div>
)

const AppRedirectUri = () => (
   <div>
      <label className="dc-label">
         Redirect Uri<span className="dc-label__sub">required</span>
      </label>
      <input type="input" className="dc-input" required="required" />
   </div>
)

const AppType = () => (
   <div>
      <label className="dc-label">
         Application Type
      </label>
      <select className="dc-select dc-select--small">
         <option value="public">public</option>
         <option value="confidential">confidential</option>
      </select>
   </div>
)

const AppKey = ({title, value}) => (
   <div>
      <label className="dc-label">{title}</label>
      <p>{value}</p>
   </div>
)

//
//
export const Register = () => (
   <DialogWithAccept
      title="Register"
      subtitle="New OAuth App"
      accept="Register"
   >
      <AppName />
      <AppRedirectUri />
      <AppType />
   </DialogWithAccept>
)

//
//
export const Credentials = () => (
   <DialogWithOk title="Credentials" subtitle="for OAuth App">
      <p>&nbsp;</p>
      <AppKey title="Access Key" value="xxx" />
      <AppKey title="Secret Key" value="xxx" />
   </DialogWithOk>
)


export const OAuthAppList = ({children}) => (
   <table className="dc-table dc-table--responsive">
      <thead className="dc-table__thead">
         <tr className="dc-table__tr">
            <th className="dc-table__th">Name</th>
            <th className="dc-table__th">Client ID</th>
            <th className="dc-table__th">Type</th>
            <th className="dc-table__th">Redirect Uri</th>
            <th className="dc-table__th"></th>
         </tr>
      </thead>
      <tbody className="dc-table__tbody">
         {children}
      </tbody>
   </table>   
)

export const OAuthApp = ({access, name, type, redirect_uri}) => (
   <tr className="dc-table__tr dc-table__tr--interactive">
      <td className="dc-table__td">{name}</td>
      <td className="dc-table__td">{access}</td>
      <td className="dc-table__td">{type}</td>
      <td className="dc-table__td">{redirect_uri}</td>
      <td className="dc-table__td">
         <a class="dc-btn dc-btn--link dc-btn--small dc-btn--remove">revoke</a>
      </td>
   </tr>
)
