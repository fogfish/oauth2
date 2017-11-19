import React from 'react';

export const OAuthAppList = ({children}) => (
   <table className="dc-table dc-table--responsive">
      <thead className="dc-table__thead">
         <tr className="dc-table__tr">
            <th className="dc-table__th">Name</th>
            <th className="dc-table__th">Client ID</th>
            <th className="dc-table__th">Security</th>
            <th className="dc-table__th">Redirect Uri</th>
            <th className="dc-table__th"></th>
         </tr>
      </thead>
      <tbody className="dc-table__tbody">
         {children}
      </tbody>
   </table>   
)

export const OAuthApp = ({app}) => (
   <tr className="dc-table__tr dc-table__tr--interactive">
      <td className="dc-table__td">{app.name}</td>
      <td className="dc-table__td">{app.access}</td>
      <td className="dc-table__td">{app.security}</td>
      <td className="dc-table__td">{app.redirect_uri}</td>
      <td className="dc-table__td">
         <a className="dc-btn dc-btn--link dc-btn--small dc-btn--remove">revoke</a>
      </td>
   </tr>
)
