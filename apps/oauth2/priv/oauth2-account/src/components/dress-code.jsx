import React from 'react';

//
//
export const Container = ({children}) => (
   <div className="dc-page">
      <div className="dc-container dc-container-limited">
         {children}
      </div>
   </div>
)

export const Layout = ({signout, children}) => (
   <div>
      <ul className="dc-tab dc-tab--header">
         <li className="dc-tab__element dc-tab__element--disabled dc-tab__element--header">Account</li>
         <li className="dc-tab__element dc-tab__element--right">
            <button className="dc-btn dc-btn--small dc-btn--link" onClick={signout}>Sign Out</button>
         </li>
      </ul>

      <div className="dc-row">   
         {children}
      </div>
   </div>
)

export const Tabs = ({title, children}) => (
   <div className="dc-column dc-column--large-2 dc-column--medium-2">
      <div className="dc-column__contents">
         <ul className="dc-tab dc-tab--vertical">
            <li className="dc-tab__element dc-tab__element--disabled dc-tab__element--header">{title}</li>
            {children}
         </ul>
      </div>
   </div>
)

export const View = ({children}) => (
   <div className="dc-column dc-column--large-10 dc-column--medium-10">
      <div className="dc-column__contents">
         {children}
      </div>
   </div>
)

//
//
export const Window = ({children}) => (
   <div className="dc-dialog">
      {children}
   </div>  
)


export const Dialog = ({title, subtitle, children}) => (
   <div className="dc-dialog__content">
      <div className="dc-dialog__body">
         <h3 className="dc-dialog__title">{title}</h3>
         <h4 className="dc-dialog__subtitle">{subtitle}</h4>
         {children}
      </div>
   </div>
)


export const DialogWithOk = ({title, subtitle, accept, children}) => (
   <Window>
      <Dialog title={title} subtitle={subtitle}>
         {children}
      </Dialog>
      <div className="dc-dialog__actions">
         <button className="dc-btn dc-btn--primary" onClick={accept}>OK</button>
      </div>
   </Window>
)

export const DialogWithAccept = ({title, subtitle, accept, cancel, children}) => (
   <Window>
      <Dialog title={title} subtitle={subtitle}>
         {children}
      </Dialog>
      <div className="dc-dialog__actions">
         <button className="dc-btn dc-btn--link" onClick={cancel}>Cancel</button>
         <button className="dc-btn dc-btn--primary" onClick={accept}>{title}</button>
      </div>
   </Window>
)


//
//
export const Tab = ({title}) => (
   <li className="dc-tab__element dc-tab__element--active">{title}</li>
)

