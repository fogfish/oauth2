import React from 'react';

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
         <a className="dc-btn dc-btn--link dc-btn--small dc-btn--remove">revoke</a>
      </td>
   </tr>
)
